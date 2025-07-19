

(define-non-fungible-token tree-nft uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-coordinates (err u102))
(define-constant err-already-verified (err u103))
(define-constant err-invalid-token-id (err u104))

(define-data-var last-token-id uint u0)
(define-data-var verifier-address principal tx-sender)

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
    
    (map-set tree-data token-id (merge tree {
      height: new-height,
      health-score: health-score,
      last-verified: current-height
    }))
    
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
  (begin
    (asserts! (is-eq tx-sender sender) err-owner-only)
    (nft-transfer? tree-nft token-id sender recipient)))

(define-private (is-valid-coordinates (lat (string-ascii 20)) (long (string-ascii 20)))
  (let ((lat-len (len lat))
        (long-len (len long)))
    (and
      (and (>= lat-len u1) (<= lat-len u20))
      (and (>= long-len u1) (<= long-len u20)))))


