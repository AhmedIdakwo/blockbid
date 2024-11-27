(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u403))
(define-constant ERR-INVALID-AUCTION (err u404))
(define-constant ERR-AUCTION-CLOSED (err u405))
(define-constant ERR-INSUFFICIENT-BID (err u406))
(define-constant ERR-AUCTION-ACTIVE (err u407))
(define-constant ERR-TRANSFER-FAILED (err u408))
(define-constant ERR-INVALID-PARAMS (err u409))

;; Auction states
(define-constant AUCTION-STATE-PENDING u0)
(define-constant AUCTION-STATE-ACTIVE u1)
(define-constant AUCTION-STATE-COMPLETED u2)
(define-constant AUCTION-STATE-CANCELLED u3)

;; Configuration constants
(define-constant MIN-BID-INCREMENT-PERCENT u10)
(define-constant MAX-AUCTION-DURATION u144000) ;; Approximately 1000 days
(define-constant MIN-AUCTION-DURATION u1440) ;; Approximately 10 days

;; Auction structure
(define-map auctions 
  { auction-id: uint }
  {
    item-name: (string-utf8 100),
    description: (optional (string-utf8 500)),
    start-price: uint,
    reserve-price: (optional uint),
    current-highest-bid: uint,
    highest-bidder: (optional principal),
    start-block: uint,
    end-block: uint,
    state: uint,
    creator: principal,
    settlement-amount: uint
  }
)

;; Bid tracking
(define-map bids
  { auction-id: uint, bidder: principal }
  { 
    bid-amount: uint,
    timestamp: uint
  }
)

;; Auction-specific withdrawals
(define-map withdrawals
  { auction-id: uint, bidder: principal }
  { amount: uint }
)

;; Auction ID tracker
(define-data-var next-auction-id uint u0)

;; Validate bid increment
(define-private (is-valid-bid-increment 
  (current-bid uint) 
  (new-bid uint)
)
  (let 
    (
      (min-next-bid (+ current-bid 
        (/ (* current-bid MIN-BID-INCREMENT-PERCENT) u100)
      ))
    )
    (>= new-bid min-next-bid)
  )
)

;; Validate auction parameters
(define-private (validate-auction-params
  (start-price uint)
  (reserve-price (optional uint))
  (start-block uint)
  (end-block uint)
)
  (and
    (> start-price u0)
    (< start-block end-block)
    (<= (- end-block start-block) MAX-AUCTION-DURATION)
    (>= (- end-block start-block) MIN-AUCTION-DURATION)
    (match reserve-price
      price (> price start-price)
      true)
  )
)

;; Create a new auction
(define-public (create-auction
  (item-name (string-utf8 100))
  (description (optional (string-utf8 500)))
  (start-price uint)
  (reserve-price (optional uint))
  (start-block uint)
  (end-block uint)
)
  (begin
    ;; Validate auction parameters
    (asserts! 
      (validate-auction-params 
        start-price 
        reserve-price 
        start-block 
        end-block
      ) 
      ERR-INVALID-PARAMS
    )

    ;; Get next auction ID
    (let 
      (
        (auction-id (var-get next-auction-id))
      )
      ;; Create auction entry
      (map-set auctions 
        { auction-id: auction-id }
        {
          item-name: item-name,
          description: description,
          start-price: start-price,
          reserve-price: reserve-price,
          current-highest-bid: start-price,
          highest-bidder: none,
          start-block: start-block,
          end-block: end-block,
          state: AUCTION-STATE-PENDING,
          creator: tx-sender,
          settlement-amount: u0
        }
      )

      ;; Increment auction ID
      (var-set next-auction-id (+ auction-id u1))

      ;; Return auction ID
      (ok auction-id)
    )
  )
)

;; Place a bid
(define-public (place-bid
  (auction-id uint)
  (bid-amount uint)
  (current-block uint)
)
  (let 
    (
      (auction (unwrap! 
        (map-get? auctions { auction-id: auction-id }) 
        ERR-INVALID-AUCTION
      ))
      (current-bid (get current-highest-bid auction))
    )
    ;; Auction state checks
    (asserts! 
      (is-eq (get state auction) AUCTION-STATE-ACTIVE) 
      ERR-AUCTION-CLOSED
    )
    (asserts! 
      (< current-block (get end-block auction)) 
      ERR-AUCTION-CLOSED
    )

    ;; Bid validation
    (asserts! 
      (is-valid-bid-increment current-bid bid-amount) 
      ERR-INSUFFICIENT-BID
    )

    ;; Optional reserve price check
    (match (get reserve-price auction)
      price (asserts! (>= bid-amount price) ERR-INSUFFICIENT-BID)
      true
    )

    ;; Update auction
    (map-set auctions 
      { auction-id: auction-id }
      (merge auction {
        current-highest-bid: bid-amount,
        highest-bidder: (some tx-sender)
      })
    )

    ;; Record bid
    (map-set bids
      { auction-id: auction-id, bidder: tx-sender }
      { 
        bid-amount: bid-amount,
        timestamp: current-block
      }
    )

    (ok true)
  )
)

;; Close and settle auction
(define-public (settle-auction 
  (auction-id uint)
  (current-block uint)
)
  (let 
    (
      (auction (unwrap! 
        (map-get? auctions { auction-id: auction-id }) 
        ERR-INVALID-AUCTION
      ))
      (current-highest-bidder 
        (unwrap! (get highest-bidder auction) ERR-INVALID-AUCTION)
      )
    )
    ;; Auction closure checks
    (asserts! 
      (>= current-block (get end-block auction)) 
      ERR-AUCTION-ACTIVE
    )
    (asserts! 
      (is-eq (get state auction) AUCTION-STATE-ACTIVE) 
      ERR-AUCTION-CLOSED
    )

    ;; Update auction state
    (map-set auctions
      { auction-id: auction-id }
      (merge auction {
        state: AUCTION-STATE-COMPLETED,
        settlement-amount: (get current-highest-bid auction)
      })
    )

    ;; Transfer funds to auction creator
    (try! (as-contract 
      (stx-transfer? 
        (get current-highest-bid auction)
        tx-sender 
        (get creator auction)
      )
    ))

    (ok true)
  )
)

;; Enable auction activation
(define-public (activate-auction 
  (auction-id uint)
  (current-block uint)
)
  (let 
    (
      (auction (unwrap! 
        (map-get? auctions { auction-id: auction-id }) 
        ERR-INVALID-AUCTION
      ))
    )
    ;; Authorization check
    (asserts! 
      (is-eq tx-sender (get creator auction)) 
      ERR-UNAUTHORIZED
    )
    ;; State and timing checks
    (asserts! 
      (is-eq (get state auction) AUCTION-STATE-PENDING) 
      ERR-AUCTION-ACTIVE
    )
    (asserts! 
      (>= current-block (get start-block auction)) 
      ERR-INVALID-AUCTION
    )

    ;; Activate auction
    (map-set auctions
      { auction-id: auction-id }
      (merge auction {
        state: AUCTION-STATE-ACTIVE
      })
    )

    (ok true)
  )
)

;; Withdraw outbid funds
(define-public (withdraw-bid (auction-id uint))
  (let 
    (
      (bid (unwrap! 
        (map-get? bids { auction-id: auction-id, bidder: tx-sender }) 
        ERR-INVALID-AUCTION
      ))
      (auction (unwrap! 
        (map-get? auctions { auction-id: auction-id }) 
        ERR-INVALID-AUCTION
      ))
    )
    ;; Check if bid is no longer highest
    (asserts! 
      (not (is-eq 
        (some tx-sender) 
        (get highest-bidder auction)
      )) 
      ERR-UNAUTHORIZED
    )

    ;; Transfer bid amount back to bidder
    (try! (as-contract 
      (stx-transfer? 
        (get bid-amount bid)
        tx-sender 
        tx-sender
      )
    ))

    (ok true)
  )
)

;; Read-only functions for auction details
(define-read-only (get-auction-details (auction-id uint))
  (map-get? auctions { auction-id: auction-id })
)

(define-read-only (get-auction-state (auction-id uint))
  (match (map-get? auctions { auction-id: auction-id })
    auction (some (get state auction))
    none
  )
)

