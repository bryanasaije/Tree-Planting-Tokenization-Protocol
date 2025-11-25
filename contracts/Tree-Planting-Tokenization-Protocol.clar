

(define-non-fungible-token tree-nft uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-coordinates (err u102))
(define-constant err-already-verified (err u103))
(define-constant err-invalid-token-id (err u104))
(define-constant err-insufficient-credits (err u105))
(define-constant err-insufficient-payment (err u106))
(define-constant err-listing-not-found (err u107))
(define-constant err-cannot-buy-own-listing (err u108))
(define-constant err-insurance-not-found (err u109))
(define-constant err-claim-already-filed (err u110))
(define-constant err-insufficient-pool-funds (err u111))
(define-constant err-tree-too-healthy (err u112))
(define-constant err-insurance-expired (err u113))
(define-constant err-invalid-royalty-rate (err u114))
(define-constant err-royalty-payment-failed (err u115))
(define-constant err-no-original-planter (err u116))

(define-data-var last-token-id uint u0)
(define-data-var verifier-address principal tx-sender)
(define-data-var last-listing-id uint u0)
(define-data-var insurance-pool uint u0)
(define-data-var last-insurance-id uint u0)
(define-data-var health-degradation-rate uint u5)
(define-data-var nft-royalty-rate uint u5)
(define-data-var credit-royalty-rate uint u3)

(define-map tree-data uint 
  {
    owner: principal,
    latitude: (string-ascii 20),
    longitude: (string-ascii 20),
    planting-date: uint,
    last-verified: uint,
    height: uint,
    health-score: uint,
    carbon-credits: uint
  }
)

(define-map verification-history uint (list 10 {
    stacks-block-height: uint,
    height: uint,
    health-score: uint
  })
)

(define-map carbon-credit-listings uint {
    token-id: uint,
    seller: principal,
    credits-amount: uint,
    price-per-credit: uint,
    active: bool
  })

(define-map tree-insurance uint {
    token-id: uint,
    owner: principal,
    coverage-amount: uint,
    premium-paid: uint,
    expiry-block: uint,
    claim-filed: bool
  })

(define-map health-snapshots uint {
    last-check-block: uint,
    last-health-score: uint
  })

(define-map tree-royalties uint {
    original-planter: principal,
    total-royalties-earned: uint,
    transfer-count: uint
  })

(define-map planter-earnings principal uint)

(define-public (set-verifier (new-verifier principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (var-set verifier-address new-verifier))))

(define-read-only (get-token-uri (token-id uint))
  (ok (some "https://treeprotocol.org/metadata/")))
(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? tree-nft token-id)))

(define-read-only (get-tree-data (token-id uint))
  (match (map-get? tree-data token-id)
    tree-info (ok tree-info)
    err-not-found))

(define-read-only (get-verification-history (token-id uint))
  (match (map-get? verification-history token-id)
    history (ok history)
    err-not-found))

(define-public (plant-tree 
    (latitude (string-ascii 20))
    (longitude (string-ascii 20)))
  (let
    ((token-id (+ (var-get last-token-id) u1)))
    (asserts! (is-valid-coordinates latitude longitude) err-invalid-coordinates)
    (try! (nft-mint? tree-nft token-id tx-sender))
    (map-set tree-data token-id {
      owner: tx-sender,
      latitude: latitude,
      longitude: longitude,
      planting-date: stacks-block-height,
      last-verified: u0,
      height: u0,
      health-score: u100,
      carbon-credits: u0
    })
    (map-set tree-royalties token-id {
      original-planter: tx-sender,
      total-royalties-earned: u0,
      transfer-count: u0
    })
    (var-set last-token-id token-id)
    (ok token-id)))

(define-public (verify-tree-growth
    (token-id uint)
    (new-height uint)
    (health-score uint))
  (let ((tree (unwrap! (map-get? tree-data token-id) err-not-found))
        (current-height stacks-block-height))
    (asserts! (is-eq tx-sender (var-get verifier-address)) err-owner-only)
    (asserts! (> new-height (get height tree)) err-invalid-token-id)
    (asserts! (<= health-score u100) err-invalid-token-id)
    
    (let ((earned-credits (/ new-height u10)))
      (map-set tree-data token-id (merge tree {
        height: new-height,
        health-score: health-score,
        last-verified: current-height,
        carbon-credits: (+ (get carbon-credits tree) earned-credits)
      })))
    
    (match (map-get? verification-history token-id)
      history (map-set verification-history token-id 
        (unwrap! (as-max-len? 
          (append history {
            stacks-block-height: current-height,
            height: new-height,
            health-score: health-score
          }) u10) err-already-verified))
      (map-set verification-history token-id (list {
        stacks-block-height: current-height,
        height: new-height,
        health-score: health-score
      })))
    
    (ok true)))

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (let ((tree (unwrap! (map-get? tree-data token-id) err-not-found))
        (royalty-info (unwrap! (map-get? tree-royalties token-id) err-no-original-planter)))
    (asserts! (is-eq tx-sender sender) err-owner-only)
    (try! (nft-transfer? tree-nft token-id sender recipient))
    (map-set tree-data token-id (merge tree { owner: recipient }))
    (map-set tree-royalties token-id (merge royalty-info {
      transfer-count: (+ (get transfer-count royalty-info) u1)
    }))
    (ok true)))

(define-read-only (get-carbon-credit-listing (listing-id uint))
  (match (map-get? carbon-credit-listings listing-id)
    listing (ok listing)
    err-listing-not-found))

(define-public (list-carbon-credits
    (token-id uint)
    (credits-amount uint)
    (price-per-credit uint))
  (let ((tree (unwrap! (map-get? tree-data token-id) err-not-found))
        (listing-id (+ (var-get last-listing-id) u1)))
    (asserts! (is-eq tx-sender (get owner tree)) err-owner-only)
    (asserts! (>= (get carbon-credits tree) credits-amount) err-insufficient-credits)
    (asserts! (> price-per-credit u0) err-invalid-token-id)
    
    (map-set tree-data token-id (merge tree {
      carbon-credits: (- (get carbon-credits tree) credits-amount)
    }))
    
    (map-set carbon-credit-listings listing-id {
      token-id: token-id,
      seller: tx-sender,
      credits-amount: credits-amount,
      price-per-credit: price-per-credit,
      active: true
    })
    
    (var-set last-listing-id listing-id)
    (ok listing-id)))

(define-public (buy-carbon-credits (listing-id uint) (credits-to-buy uint))
  (let ((listing (unwrap! (map-get? carbon-credit-listings listing-id) err-listing-not-found))
        (total-price (* credits-to-buy (get price-per-credit listing)))
        (royalty-info (unwrap! (map-get? tree-royalties (get token-id listing)) err-no-original-planter))
        (royalty-amount (/ (* total-price (var-get credit-royalty-rate)) u100))
        (seller-amount (- total-price royalty-amount)))
    (asserts! (get active listing) err-listing-not-found)
    (asserts! (not (is-eq tx-sender (get seller listing))) err-cannot-buy-own-listing)
    (asserts! (<= credits-to-buy (get credits-amount listing)) err-insufficient-credits)
    
    (try! (stx-transfer? seller-amount tx-sender (get seller listing)))
    (try! (stx-transfer? royalty-amount tx-sender (get original-planter royalty-info)))
    
    (map-set tree-royalties (get token-id listing) (merge royalty-info {
      total-royalties-earned: (+ (get total-royalties-earned royalty-info) royalty-amount)
    }))
    
    (match (map-get? planter-earnings (get original-planter royalty-info))
      current-earnings (map-set planter-earnings (get original-planter royalty-info) (+ current-earnings royalty-amount))
      (map-set planter-earnings (get original-planter royalty-info) royalty-amount))
    
    (if (is-eq credits-to-buy (get credits-amount listing))
      (map-set carbon-credit-listings listing-id (merge listing { active: false }))
      (map-set carbon-credit-listings listing-id (merge listing {
        credits-amount: (- (get credits-amount listing) credits-to-buy)
      })))
    
    (ok total-price)))

(define-public (cancel-listing (listing-id uint))
  (let ((listing (unwrap! (map-get? carbon-credit-listings listing-id) err-listing-not-found))
        (tree (unwrap! (map-get? tree-data (get token-id listing)) err-not-found)))
    (asserts! (is-eq tx-sender (get seller listing)) err-owner-only)
    (asserts! (get active listing) err-listing-not-found)
    
    (map-set tree-data (get token-id listing) (merge tree {
      carbon-credits: (+ (get carbon-credits tree) (get credits-amount listing))
    }))
    
    (map-set carbon-credit-listings listing-id (merge listing { active: false }))
    
    (ok true)))

(define-public (purchase-insurance 
    (token-id uint)
    (coverage-amount uint)
    (duration-blocks uint))
  (let ((tree (unwrap! (map-get? tree-data token-id) err-not-found))
        (insurance-id (+ (var-get last-insurance-id) u1))
        (premium (calculate-premium coverage-amount duration-blocks))
        (expiry-block (+ stacks-block-height duration-blocks)))
    (asserts! (is-eq tx-sender (get owner tree)) err-owner-only)
    (asserts! (> coverage-amount u0) err-invalid-token-id)
    
    (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
    (var-set insurance-pool (+ (var-get insurance-pool) premium))
    
    (map-set tree-insurance insurance-id {
      token-id: token-id,
      owner: tx-sender,
      coverage-amount: coverage-amount,
      premium-paid: premium,
      expiry-block: expiry-block,
      claim-filed: false
    })
    
    (map-set health-snapshots token-id {
      last-check-block: stacks-block-height,
      last-health-score: (get health-score tree)
    })
    
    (var-set last-insurance-id insurance-id)
    (ok insurance-id)))

(define-public (file-health-claim (insurance-id uint))
  (let ((insurance (unwrap! (map-get? tree-insurance insurance-id) err-insurance-not-found))
        (tree (unwrap! (map-get? tree-data (get token-id insurance)) err-not-found))
        (snapshot (unwrap! (map-get? health-snapshots (get token-id insurance)) err-not-found)))
    (asserts! (is-eq tx-sender (get owner insurance)) err-owner-only)
    (asserts! (not (get claim-filed insurance)) err-claim-already-filed)
    (asserts! (<= stacks-block-height (get expiry-block insurance)) err-insurance-expired)
    
    (let ((current-health (calculate-current-health (get token-id insurance) snapshot))
          (coverage (get coverage-amount insurance)))
      (asserts! (<= current-health u30) err-tree-too-healthy)
      (asserts! (>= (var-get insurance-pool) coverage) err-insufficient-pool-funds)
      
      (try! (as-contract (stx-transfer? coverage tx-sender (get owner insurance))))
      (var-set insurance-pool (- (var-get insurance-pool) coverage))
      
      (map-set tree-insurance insurance-id (merge insurance {
        claim-filed: true
      }))
      
      (ok coverage))))

(define-read-only (get-insurance-details (insurance-id uint))
  (match (map-get? tree-insurance insurance-id)
    insurance (ok insurance)
    err-insurance-not-found))

(define-private (calculate-current-health (token-id uint) (snapshot {last-check-block: uint, last-health-score: uint}))
  (let ((blocks-passed (- stacks-block-height (get last-check-block snapshot)))
        (degradation (* blocks-passed (var-get health-degradation-rate)))
        (current-health (if (> degradation (get last-health-score snapshot))
                          u0
                          (- (get last-health-score snapshot) degradation))))
    current-health))

(define-public (update-health-snapshot (token-id uint))
  (let ((tree (unwrap! (map-get? tree-data token-id) err-not-found)))
    (map-set health-snapshots token-id {
      last-check-block: stacks-block-height,
      last-health-score: (get health-score tree)
    })
    (ok true)))

(define-read-only (check-tree-health (token-id uint))
  (match (map-get? health-snapshots token-id)
    snapshot (ok (calculate-current-health token-id snapshot))
    err-not-found))

(define-public (fund-insurance-pool (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set insurance-pool (+ (var-get insurance-pool) amount))
    (ok amount)))

(define-private (calculate-premium (coverage-amount uint) (duration-blocks uint))
  (let ((base-rate u1000)
        (duration-factor (/ duration-blocks u1000))
        (coverage-factor (/ coverage-amount u1000000)))
    (+ base-rate (* duration-factor coverage-factor))))

(define-read-only (get-insurance-pool-balance)
  (ok (var-get insurance-pool)))

(define-public (set-health-degradation-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set health-degradation-rate new-rate)
    (ok new-rate)))

(define-private (is-valid-coordinates (lat (string-ascii 20)) (long (string-ascii 20)))
  (let ((lat-len (len lat))
        (long-len (len long)))
    (and
      (and (>= lat-len u1) (<= lat-len u20))
      (and (>= long-len u1) (<= long-len u20)))))

(define-read-only (get-royalty-info (token-id uint))
  (match (map-get? tree-royalties token-id)
    royalty-data (ok royalty-data)
    err-not-found))

(define-read-only (get-planter-total-earnings (planter principal))
  (ok (default-to u0 (map-get? planter-earnings planter))))

(define-read-only (get-royalty-rates)
  (ok {
    nft-rate: (var-get nft-royalty-rate),
    credit-rate: (var-get credit-royalty-rate)
  }))

(define-public (set-royalty-rates (nft-rate uint) (credit-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= nft-rate u20) err-invalid-royalty-rate)
    (asserts! (<= credit-rate u20) err-invalid-royalty-rate)
    (var-set nft-royalty-rate nft-rate)
    (var-set credit-royalty-rate credit-rate)
    (ok true)))

(define-public (transfer-with-royalty (token-id uint) (sender principal) (recipient principal) (sale-price uint))
  (let ((tree (unwrap! (map-get? tree-data token-id) err-not-found))
        (royalty-info (unwrap! (map-get? tree-royalties token-id) err-no-original-planter))
        (royalty-amount (/ (* sale-price (var-get nft-royalty-rate)) u100))
        (seller-amount (- sale-price royalty-amount)))
    (asserts! (is-eq tx-sender sender) err-owner-only)
    (asserts! (> sale-price u0) err-insufficient-payment)
    
    (try! (stx-transfer? seller-amount recipient sender))
    (try! (stx-transfer? royalty-amount recipient (get original-planter royalty-info)))
    
    (try! (nft-transfer? tree-nft token-id sender recipient))
    (map-set tree-data token-id (merge tree { owner: recipient }))
    
    (map-set tree-royalties token-id (merge royalty-info {
      total-royalties-earned: (+ (get total-royalties-earned royalty-info) royalty-amount),
      transfer-count: (+ (get transfer-count royalty-info) u1)
    }))
    
    (match (map-get? planter-earnings (get original-planter royalty-info))
      current-earnings (map-set planter-earnings (get original-planter royalty-info) (+ current-earnings royalty-amount))
      (map-set planter-earnings (get original-planter royalty-info) royalty-amount))
    
    (ok royalty-amount)))


