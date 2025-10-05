;; NanoMedia - Decentralized Media Licensing Platform
;; A smart contract for fractional media licensing and royalty distribution

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-payment (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-parameters (err u105))
(define-constant err-license-expired (err u106))

;; Data Variables
(define-data-var platform-fee-percentage uint u250) ;; 2.5% (basis points)
(define-data-var next-media-id uint u1)
(define-data-var next-license-id uint u1)

;; Data Maps

;; Media Asset Storage
(define-map media-assets
    uint ;; media-id
    {
        creator: principal,
        ipfs-hash: (string-ascii 64),
        title: (string-utf8 256),
        base-price: uint,
        reputation-score: uint,
        total-licenses-sold: uint,
        total-revenue: uint,
        is-active: bool,
        created-at: uint
    }
)

;; License Records
(define-map licenses
    uint ;; license-id
    {
        media-id: uint,
        licensee: principal,
        license-type: (string-ascii 32), ;; "commercial", "personal", "remix", etc.
        duration: uint, ;; in blocks
        geographic-scope: (string-ascii 64),
        max-audience-size: uint,
        price-paid: uint,
        expiry-block: uint,
        is-active: bool,
        purchased-at: uint
    }
)

;; Royalty Stakeholders (supports collaborative works)
(define-map royalty-splits
    { media-id: uint, stakeholder: principal }
    { percentage: uint } ;; basis points (10000 = 100%)
)

;; Dynamic Pricing Factors
(define-map pricing-multipliers
    uint ;; media-id
    {
        demand-multiplier: uint, ;; basis points
        performance-multiplier: uint, ;; basis points
        last-updated: uint
    }
)

;; User Reputation
(define-map user-reputation
    principal
    {
        score: uint,
        total-sales: uint,
        disputes-resolved: uint
    }
)

;; Read-only functions

(define-read-only (get-media-asset (media-id uint))
    (map-get? media-assets media-id)
)

(define-read-only (get-license (license-id uint))
    (map-get? licenses license-id)
)

(define-read-only (get-royalty-split (media-id uint) (stakeholder principal))
    (default-to 
        { percentage: u0 }
        (map-get? royalty-splits { media-id: media-id, stakeholder: stakeholder })
    )
)

(define-read-only (get-dynamic-price (media-id uint))
    (let (
        (asset (unwrap! (get-media-asset media-id) (err err-not-found)))
        (multipliers (default-to 
            { demand-multiplier: u10000, performance-multiplier: u10000, last-updated: u0 }
            (map-get? pricing-multipliers media-id)
        ))
        (base-price (get base-price asset))
        (demand-mult (get demand-multiplier multipliers))
        (perf-mult (get performance-multiplier multipliers))
    )
    (ok (/ (* (* base-price demand-mult) perf-mult) u100000000))
    )
)

(define-read-only (is-license-valid (license-id uint))
    (match (get-license license-id)
        license-data (and 
            (get is-active license-data)
            (< block-height (get expiry-block license-data))
        )
        false
    )
)

(define-read-only (get-platform-fee-percentage)
    (ok (var-get platform-fee-percentage))
)

(define-read-only (get-user-reputation (user principal))
    (default-to
        { score: u100, total-sales: u0, disputes-resolved: u0 }
        (map-get? user-reputation user)
    )
)

;; Public functions

;; Register a new media asset
(define-public (register-media-asset 
    (ipfs-hash (string-ascii 64))
    (title (string-utf8 256))
    (base-price uint)
)
    (let (
        (media-id (var-get next-media-id))
    )
    (asserts! (> base-price u0) err-invalid-parameters)
    (map-set media-assets media-id {
        creator: tx-sender,
        ipfs-hash: ipfs-hash,
        title: title,
        base-price: base-price,
        reputation-score: u100,
        total-licenses-sold: u0,
        total-revenue: u0,
        is-active: true,
        created-at: block-height
    })
    (map-set pricing-multipliers media-id {
        demand-multiplier: u10000,
        performance-multiplier: u10000,
        last-updated: block-height
    })
    ;; Creator gets 100% royalty by default
    (map-set royalty-splits 
        { media-id: media-id, stakeholder: tx-sender }
        { percentage: u10000 }
    )
    (var-set next-media-id (+ media-id u1))
    (ok media-id)
    )
)

;; Purchase a license
(define-public (purchase-license
    (media-id uint)
    (license-type (string-ascii 32))
    (duration uint)
    (geographic-scope (string-ascii 64))
    (max-audience-size uint)
)
    (let (
        (asset (unwrap! (get-media-asset media-id) err-not-found))
        (dynamic-price (unwrap! (get-dynamic-price media-id) err-not-found))
        (platform-fee (/ (* dynamic-price (var-get platform-fee-percentage)) u10000))
        (creator-payment (- dynamic-price platform-fee))
        (license-id (var-get next-license-id))
        (expiry-block (+ block-height duration))
    )
    (asserts! (get is-active asset) err-not-found)
    (asserts! (> duration u0) err-invalid-parameters)
    
    ;; Transfer payment from buyer
    (try! (stx-transfer? dynamic-price tx-sender (as-contract tx-sender)))
    
    ;; Create license record
    (map-set licenses license-id {
        media-id: media-id,
        licensee: tx-sender,
        license-type: license-type,
        duration: duration,
        geographic-scope: geographic-scope,
        max-audience-size: max-audience-size,
        price-paid: dynamic-price,
        expiry-block: expiry-block,
        is-active: true,
        purchased-at: block-height
    })
    
    ;; Update media asset stats
    (map-set media-assets media-id (merge asset {
        total-licenses-sold: (+ (get total-licenses-sold asset) u1),
        total-revenue: (+ (get total-revenue asset) dynamic-price)
    }))
    
    ;; Distribute royalties to creator (simplified - single stakeholder)
    (try! (as-contract (stx-transfer? creator-payment tx-sender (get creator asset))))
    
    ;; Update pricing multiplier based on demand
    (try! (update-demand-multiplier media-id))
    
    (var-set next-license-id (+ license-id u1))
    (ok license-id)
    )
)

;; Add royalty stakeholder for collaborative works
(define-public (add-royalty-stakeholder
    (media-id uint)
    (stakeholder principal)
    (percentage uint)
)
    (let (
        (asset (unwrap! (get-media-asset media-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator asset)) err-unauthorized)
    (asserts! (<= percentage u10000) err-invalid-parameters)
    (map-set royalty-splits 
        { media-id: media-id, stakeholder: stakeholder }
        { percentage: percentage }
    )
    (ok true)
    )
)

;; Update demand multiplier based on recent sales
(define-public (update-demand-multiplier (media-id uint))
    (let (
        (asset (unwrap! (get-media-asset media-id) err-not-found))
        (current-multipliers (unwrap! (map-get? pricing-multipliers media-id) err-not-found))
        (sales-count (get total-licenses-sold asset))
        ;; Simple demand formula: increase by 1% for every 10 sales
        (new-multiplier (+ u10000 (* (/ sales-count u10) u100)))
    )
    (map-set pricing-multipliers media-id (merge current-multipliers {
        demand-multiplier: new-multiplier,
        last-updated: block-height
    }))
    (ok true)
    )
)

;; Revoke a license (by creator or owner)
(define-public (revoke-license (license-id uint))
    (let (
        (license-data (unwrap! (get-license license-id) err-not-found))
        (asset (unwrap! (get-media-asset (get media-id license-data)) err-not-found))
    )
    (asserts! 
        (or 
            (is-eq tx-sender (get creator asset))
            (is-eq tx-sender (get licensee license-data))
        ) 
        err-unauthorized
    )
    (map-set licenses license-id (merge license-data { is-active: false }))
    (ok true)
    )
)

;; Deactivate media asset
(define-public (deactivate-media-asset (media-id uint))
    (let (
        (asset (unwrap! (get-media-asset media-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator asset)) err-unauthorized)
    (map-set media-assets media-id (merge asset { is-active: false }))
    (ok true)
    )
)

;; Update platform fee (owner only)
(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-fee u1000) err-invalid-parameters) ;; Max 10%
        (var-set platform-fee-percentage new-fee)
        (ok true)
    )
)

;; Update user reputation (simplified version)
(define-public (update-reputation (user principal) (score-delta int))
    (let (
        (current-rep (get-user-reputation user))
        (current-score (get score current-rep))
        (new-score (if (> score-delta 0)
            (+ current-score (to-uint score-delta))
            (if (> current-score (to-uint (* score-delta -1)))
                (- current-score (to-uint (* score-delta -1)))
                u0
            )
        ))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set user-reputation user (merge current-rep { score: new-score }))
    (ok true)
    )
)
