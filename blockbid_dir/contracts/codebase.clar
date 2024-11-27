(define-constant contract-owner tx-sender)
(define-constant min-bid-increment u10)
(define-constant auction-duration u14400) ;; 4 hours in blocks
(define-constant err-invalid-start-price (err u1))
(define-constant err-auction-not-found (err u2))
(define-constant err-auction-inactive (err u3))
(define-constant err-auction-expired (err u4))
(define-constant err-insufficient-bid (err u5))
(define-constant err-auction-lookup-failed (err u6))
(define-constant err-auction-not-closable (err u7))
(define-constant err-auction-already-closed (err u8))

(define-map auctions 
  { auction-id: uint }
  {
    item-name: (string-utf8 100),
    start-price: uint,
    current-highest-bid: uint,
    highest-bidder: principal,
    start-block: uint,
    is-active: bool,
    owner: principal
  }
)

(define-map bids 
  { auction-id: uint, bidder: principal }
  { bid-amount: uint }
)

(define-data-var next-auction-id uint u0)

;; Validate bid amount
(define-private (is-valid-bid 
  (current-bid uint) 
  (new-bid uint)
)
  (and 
    (> new-bid current-bid)
    (>= new-bid (+ current-bid min-bid-increment))
  )
)

;; Create a new auction
(define-public (create-auction 
  (item-name (string-utf8 100)) 
  (start-price uint)
)
  (begin
    (asserts! (> start-price u0) err-invalid-start-price)
    (let 
      (
        (auction-id (var-get next-auction-id))
      )
      (map-set auctions 
        { auction-id: auction-id }
        {
          item-name: item-name,
          start-price: start-price,
          current-highest-bid: start-price,
          highest-bidder: contract-owner,
          start-block: block-height,
          is-active: true,
          owner: tx-sender
        }
      )
      (var-set next-auction-id (+ auction-id u1))
      (ok auction-id)
    )
  )
)

;; Refund previous highest bidder
(define-private (refund-previous-bidder 
  (auction-id uint) 
  (current-auction (tuple (current-highest-bid uint) (highest-bidder principal)))
)
  (if (not (is-eq (get highest-bidder current-auction) contract-owner))
    (stx-transfer? 
      (get current-highest-bid current-auction) 
      (as-contract tx-sender) 
      (get highest-bidder current-auction)
    )
    (ok true)
  )
)

;; Place a bid
(define-public (place-bid 
  (auction-id uint) 
  (bid-amount uint)
)
  (let 
    (
      (auction (unwrap! 
        (map-get? auctions { auction-id: auction-id }) 
        err-auction-not-found
      ))
      (current-bid (get current-highest-bid auction))
      (elapsed-blocks (- block-height (get start-block auction)))
    )
    (asserts! (get is-active auction) err-auction-inactive)
    (asserts! (< elapsed-blocks auction-duration) err-auction-expired)
    (asserts! 
      (is-valid-bid current-bid bid-amount) 
      err-insufficient-bid
    )
    
    ;; Refund previous highest bidder
    (unwrap! 
      (refund-previous-bidder 
        auction-id 
        { 
          current-highest-bid: current-bid, 
          highest-bidder: (get highest-bidder auction) 
        }
      ) 
      err-auction-lookup-failed
    )
    
    ;; Update auction details
    (map-set auctions 
      { auction-id: auction-id }
      (merge auction {
        current-highest-bid: bid-amount,
        highest-bidder: tx-sender
      })
    )
    
    ;; Record bid
    (map-set bids 
      { auction-id: auction-id, bidder: tx-sender }
      { bid-amount: bid-amount }
    )
    
    (ok true)
  )
)

;; Close auction and transfer funds
(define-public (close-auction (auction-id uint))
  (let 
    (
      (auction (unwrap! 
        (map-get? auctions { auction-id: auction-id }) 
        err-auction-lookup-failed
      ))
      (elapsed-blocks (- block-height (get start-block auction)))
    )
    (asserts! (>= elapsed-blocks auction-duration) err-auction-not-closable)
    (asserts! (get is-active auction) err-auction-already-closed)
    
    ;; Mark auction as inactive
    (map-set auctions 
      { auction-id: auction-id }
      (merge auction { is-active: false })
    )
    
    ;; Transfer highest bid to auction owner
    (as-contract 
      (stx-transfer? 
        (get current-highest-bid auction) 
        (get owner auction) 
        (get owner auction)
      )
    )
    
    (ok true)
  )
)

;; Retrieve auction details
(define-read-only (get-auction-details (auction-id uint))
  (map-get? auctions { auction-id: auction-id })
)

;; Get current highest bid
(define-read-only (get-current-highest-bid (auction-id uint))
  (match (map-get? auctions { auction-id: auction-id })
    auction (some (get current-highest-bid auction))
    none
  )
)