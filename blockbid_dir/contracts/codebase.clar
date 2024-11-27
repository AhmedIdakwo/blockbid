(define-constant contract-owner tx-sender)
(define-constant min-bid-increment u10)
(define-constant auction-duration u14400) ;; 4 hours in blocks

(define-map auctions 
  { auction-id: uint }
  {
    item-name: (string-utf8 100),
    start-price: uint,
    current-highest-bid: uint,
    highest-bidder: principal,
    start-block: uint,
    is-active: bool
  }
)

(define-map bids 
  { auction-id: uint, bidder: principal }
  { bid-amount: uint }
)

(define-data-var next-auction-id uint u0)

;; Create a new auction
(define-public (create-auction 
  (item-name (string-utf8 100)) 
  (start-price uint)
)
  (let 
    (
      (auction-id (var-get next-auction-id))
    )
    (asserts! (> start-price u0) (err u1))
    (map-set auctions 
      { auction-id: auction-id }
      {
        item-name: item-name,
        start-price: start-price,
        current-highest-bid: start-price,
        highest-bidder: contract-owner,
        start-block: block-height,
        is-active: true
      }
    )
    (var-set next-auction-id (+ auction-id u1))
    (ok auction-id)
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
        (err u2)
      ))
      (current-bid (get current-highest-bid auction))
      (elapsed-blocks (- block-height (get start-block auction)))
    )
    (asserts! (get is-active auction) (err u3))
    (asserts! (< elapsed-blocks auction-duration) (err u4))
    (asserts! (>= bid-amount (+ current-bid min-bid-increment)) (err u5))
    
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
        (err u6)
      ))
      (elapsed-blocks (- block-height (get start-block auction)))
    )
    (asserts! (>= elapsed-blocks auction-duration) (err u7))
    (asserts! (get is-active auction) (err u8))
    
    ;; Mark auction as inactive
    (map-set auctions 
      { auction-id: auction-id }
      (merge auction { is-active: false })
    )
    
    ;; Transfer highest bid to contract owner
    (as-contract 
      (stx-transfer? 
        (get current-highest-bid auction) 
        contract-owner 
        contract-owner
      )
    )
    
    (ok true)
  )
)

;; Retrieve auction details
(define-read-only (get-auction-details (auction-id uint))
  (map-get? auctions { auction-id: auction-id })
)