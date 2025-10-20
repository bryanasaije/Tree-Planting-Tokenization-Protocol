;; Tree Planting Tokenization Protocol
;; A comprehensive smart contract for tokenizing tree planting activities
;; Features: Tree NFTs, Carbon Credits, Planting Verification, Impact Rewards

;; === CONSTANTS ===
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-TREE-ID (err u101))
(define-constant ERR-TREE-ALREADY-EXISTS (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))
(define-constant ERR-INVALID-AMOUNT (err u104))
(define-constant ERR-TREE-NOT-VERIFIED (err u105))
(define-constant ERR-ALREADY-VERIFIED (err u106))
(define-constant ERR-INVALID-LOCATION (err u107))
(define-constant ERR-REWARD-ALREADY-CLAIMED (err u108))

;; === DATA STRUCTURES ===

;; Tree registry with comprehensive metadata
(define-map trees 
  { tree-id: uint }
  {
    owner: principal,
    species: (string-ascii 50),
    location-lat: int,
    location-lon: int,
    planting-date: uint,
    verified: bool,
    verifier: (optional principal),
    estimated-carbon-sequestration: uint, ;; in kg CO2 over lifetime
    current-growth-stage: (string-ascii 20),
    health-status: (string-ascii 20)
  }
)

;; Carbon credit balances (fungible tokens)
(define-map carbon-credits { owner: principal } { balance: uint })

;; Planting activity tracking
(define-map planting-records
  { planter: principal, season: uint }
  { 
    trees-planted: uint,
    total-carbon-potential: uint,
    verified-trees: uint,
    reward-claimed: bool
  }
)

;; Verifier registry
(define-map authorized-verifiers { verifier: principal } { active: bool })

;; === STATE VARIABLES ===
(define-data-var next-tree-id uint u1)
(define-data-var total-trees-planted uint u0)
(define-data-var total-carbon-credits-issued uint u0)
(define-data-var current-season uint u1)
(define-data-var carbon-price-per-kg uint u10) ;; Base price in microSTX

;; === PRIVATE FUNCTIONS ===

(define-private (is-contract-owner (user principal))
  (is-eq user CONTRACT-OWNER)
)

(define-private (is-authorized-verifier (user principal))
  (default-to false 
    (get active (map-get? authorized-verifiers { verifier: user }))
  )
)

(define-private (calculate-carbon-credits (sequestration uint))
  ;; Convert estimated carbon sequestration to credits (1 credit = 1kg CO2)
  sequestration
)

(define-private (calculate-seasonal-reward (trees-planted uint) (verified-trees uint))
  ;; Reward formula: base reward * trees * verification bonus
  (let (
    (base-reward u1000) ;; microSTX per tree
    (verification-bonus (if (>= verified-trees (/ trees-planted u2)) u2 u1))
  )
    (* (* base-reward trees-planted) verification-bonus)
  )
)

;; === ADMIN FUNCTIONS ===

(define-public (add-verifier (verifier principal))
  (begin
    (asserts! (is-contract-owner tx-sender) ERR-NOT-AUTHORIZED)
    (ok (map-set authorized-verifiers 
      { verifier: verifier } 
      { active: true }
    ))
  )
)

(define-public (remove-verifier (verifier principal))
  (begin
    (asserts! (is-contract-owner tx-sender) ERR-NOT-AUTHORIZED)
    (ok (map-set authorized-verifiers 
      { verifier: verifier } 
      { active: false }
    ))
  )
)

(define-public (update-carbon-price (new-price uint))
  (begin
    (asserts! (is-contract-owner tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> new-price u0) ERR-INVALID-AMOUNT)
    (ok (var-set carbon-price-per-kg new-price))
  )
)

(define-public (advance-season)
  (begin
    (asserts! (is-contract-owner tx-sender) ERR-NOT-AUTHORIZED)
    (ok (var-set current-season (+ (var-get current-season) u1)))
  )
)

;; === CORE FUNCTIONS ===

(define-public (plant-tree 
    (species (string-ascii 50))
    (location-lat int)
    (location-lon int)
    (estimated-carbon-sequestration uint)
  )
  (let (
    (tree-id (var-get next-tree-id))
  )
    ;; Validate inputs
    (asserts! (> (len species) u0) ERR-INVALID-AMOUNT)
    (asserts! (and (>= location-lat -90000000) (<= location-lat 90000000)) ERR-INVALID-LOCATION)
    (asserts! (and (>= location-lon -180000000) (<= location-lon 180000000)) ERR-INVALID-LOCATION)
    (asserts! (> estimated-carbon-sequestration u0) ERR-INVALID-AMOUNT)
    
    ;; Create tree record
    (map-set trees
      { tree-id: tree-id }
      {
        owner: tx-sender,
        species: species,
        location-lat: location-lat,
        location-lon: location-lon,
        planting-date: burn-block-height,
        verified: false,
        verifier: none,
        estimated-carbon-sequestration: estimated-carbon-sequestration,
        current-growth-stage: "seedling",
        health-status: "healthy"
      }
    )
    
    ;; Update planting records
    (let (
      (season (var-get current-season))
      (existing-record (default-to 
        { trees-planted: u0, total-carbon-potential: u0, verified-trees: u0, reward-claimed: false }
        (map-get? planting-records { planter: tx-sender, season: season })
      ))
    )
      (map-set planting-records
        { planter: tx-sender, season: season }
        {
          trees-planted: (+ (get trees-planted existing-record) u1),
          total-carbon-potential: (+ (get total-carbon-potential existing-record) estimated-carbon-sequestration),
          verified-trees: (get verified-trees existing-record),
          reward-claimed: (get reward-claimed existing-record)
        }
      )
    )
    
    ;; Update global counters
    (var-set next-tree-id (+ tree-id u1))
    (var-set total-trees-planted (+ (var-get total-trees-planted) u1))
    
    (ok tree-id)
  )
)

(define-public (verify-tree (tree-id uint) (health-status (string-ascii 20)) (growth-stage (string-ascii 20)))
  (let (
    (tree-data (unwrap! (map-get? trees { tree-id: tree-id }) ERR-INVALID-TREE-ID))
  )
    ;; Check authorization
    (asserts! (is-authorized-verifier tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (not (get verified tree-data)) ERR-ALREADY-VERIFIED)
    
    ;; Update tree record
    (map-set trees
      { tree-id: tree-id }
      (merge tree-data {
        verified: true,
        verifier: (some tx-sender),
        health-status: health-status,
        current-growth-stage: growth-stage
      })
    )
    
    ;; Issue carbon credits to tree owner
    (let (
      (owner (get owner tree-data))
      (carbon-amount (calculate-carbon-credits (get estimated-carbon-sequestration tree-data)))
      (current-balance (default-to u0 (get balance (map-get? carbon-credits { owner: owner }))))
    )
      (map-set carbon-credits
        { owner: owner }
        { balance: (+ current-balance carbon-amount) }
      )
      
      ;; Update global carbon credits counter
      (var-set total-carbon-credits-issued (+ (var-get total-carbon-credits-issued) carbon-amount))
    )
    
    ;; Update planting records
    (let (
      (owner (get owner tree-data))
      (planting-block (get planting-date tree-data))
      ;; Estimate season based on planting date (simplified)
      (estimated-season (var-get current-season))
      (existing-record (unwrap-panic (map-get? planting-records { planter: owner, season: estimated-season })))
    )
      (map-set planting-records
        { planter: owner, season: estimated-season }
        (merge existing-record {
          verified-trees: (+ (get verified-trees existing-record) u1)
        })
      )
    )
    
    (ok true)
  )
)

(define-public (update-tree-status (tree-id uint) (health-status (string-ascii 20)) (growth-stage (string-ascii 20)))
  (let (
    (tree-data (unwrap! (map-get? trees { tree-id: tree-id }) ERR-INVALID-TREE-ID))
  )
    ;; Only owner or verifier can update
    (asserts! (or (is-eq tx-sender (get owner tree-data)) 
                  (is-authorized-verifier tx-sender)) ERR-NOT-AUTHORIZED)
    
    (ok (map-set trees
      { tree-id: tree-id }
      (merge tree-data {
        health-status: health-status,
        current-growth-stage: growth-stage
      })
    ))
  )
)

(define-public (trade-carbon-credits (recipient principal) (amount uint))
  (let (
    (sender-balance (default-to u0 (get balance (map-get? carbon-credits { owner: tx-sender }))))
    (recipient-balance (default-to u0 (get balance (map-get? carbon-credits { owner: recipient }))))
  )
    (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Update balances
    (map-set carbon-credits
      { owner: tx-sender }
      { balance: (- sender-balance amount) }
    )
    
    (map-set carbon-credits
      { owner: recipient }
      { balance: (+ recipient-balance amount) }
    )
    
    (ok true)
  )
)

(define-public (claim-seasonal-reward (season uint))
  (let (
    (planting-record (unwrap! (map-get? planting-records { planter: tx-sender, season: season }) ERR-INVALID-AMOUNT))
  )
    (asserts! (not (get reward-claimed planting-record)) ERR-REWARD-ALREADY-CLAIMED)
    (asserts! (> (get trees-planted planting-record) u0) ERR-INVALID-AMOUNT)
    
    ;; Calculate and distribute reward
    (let (
      (reward-amount (calculate-seasonal-reward 
        (get trees-planted planting-record) 
        (get verified-trees planting-record)
      ))
    )
      ;; Mark reward as claimed
      (map-set planting-records
        { planter: tx-sender, season: season }
        (merge planting-record { reward-claimed: true })
      )
      
      ;; Note: In a real implementation, you'd transfer STX here
      ;; For this example, we just return the reward amount
      (ok reward-amount)
    )
  )
)

;; === READ-ONLY FUNCTIONS ===

(define-read-only (get-tree-info (tree-id uint))
  (map-get? trees { tree-id: tree-id })
)

(define-read-only (get-carbon-balance (owner principal))
  (default-to u0 (get balance (map-get? carbon-credits { owner: owner })))
)

(define-read-only (get-planting-record (planter principal) (season uint))
  (map-get? planting-records { planter: planter, season: season })
)

(define-read-only (get-contract-stats)
  {
    total-trees: (var-get total-trees-planted),
    total-carbon-credits: (var-get total-carbon-credits-issued),
    current-season: (var-get current-season),
    next-tree-id: (var-get next-tree-id),
    carbon-price: (var-get carbon-price-per-kg)
  }
)

(define-read-only (is-verifier-authorized (verifier principal))
  (is-authorized-verifier verifier)
)
