;; Health Inspector - Public Health Inspection Smart Contract
;; This contract manages facility inspections, violation tracking, and public reporting for food safety

;; Data Variables
(define-data-var contract-owner principal tx-sender)
(define-data-var next-facility-id uint u1)
(define-data-var next-inspection-id uint u1)
(define-data-var next-violation-id uint u1)
(define-data-var current-timestamp uint u1)

;; Data Maps
(define-map facilities
  { facility-id: uint }
  {
    name: (string-ascii 100),
    owner: principal,
    address: (string-ascii 200),
    facility-type: (string-ascii 50),
    license-number: (string-ascii 50),
    registration-date: uint,
    is-active: bool
  }
)

(define-map inspections
  { inspection-id: uint }
  {
    facility-id: uint,
    inspector: principal,
    inspection-date: uint,
    inspection-type: (string-ascii 50),
    overall-score: uint,
    status: (string-ascii 20),
    notes: (string-ascii 500)
  }
)

(define-map violations
  { violation-id: uint }
  {
    inspection-id: uint,
    violation-type: (string-ascii 100),
    severity: (string-ascii 20),
    description: (string-ascii 300),
    is-corrected: bool,
    correction-date: (optional uint)
  }
)

(define-map facility-inspectors
  { inspector: principal }
  { authorized: bool }
)

;; Public Functions

;; Register a new facility
(define-public (register-facility (name (string-ascii 100)) 
                                 (address (string-ascii 200))
                                 (facility-type (string-ascii 50))
                                 (license-number (string-ascii 50)))
  (let (
    (facility-id (var-get next-facility-id))
    (timestamp (var-get current-timestamp))
  )
    (asserts! (> (len name) u0) (err u400))
    (asserts! (> (len address) u0) (err u401))
    (asserts! (> (len facility-type) u0) (err u402))
    (asserts! (> (len license-number) u0) (err u403))
    
    (map-set facilities
      { facility-id: facility-id }
      {
        name: name,
        owner: tx-sender,
        address: address,
        facility-type: facility-type,
        license-number: license-number,
        registration-date: timestamp,
        is-active: true
      }
    )
    
    (var-set next-facility-id (+ facility-id u1))
    (var-set current-timestamp (+ timestamp u1))
    (ok facility-id)
  )
)

;; Authorize an inspector (only contract owner)
(define-public (authorize-inspector (inspector principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u100))
    (map-set facility-inspectors
      { inspector: inspector }
      { authorized: true }
    )
    (ok true)
  )
)

;; Conduct an inspection (only authorized inspectors)
(define-public (conduct-inspection (facility-id uint)
                                  (inspection-type (string-ascii 50))
                                  (overall-score uint)
                                  (notes (string-ascii 500)))
  (let (
    (inspection-id (var-get next-inspection-id))
    (timestamp (var-get current-timestamp))
    (inspector-auth (default-to { authorized: false } 
                               (map-get? facility-inspectors { inspector: tx-sender })))
  )
    (asserts! (get authorized inspector-auth) (err u200))
    (asserts! (is-some (map-get? facilities { facility-id: facility-id })) (err u201))
    (asserts! (<= overall-score u100) (err u202))
    (asserts! (> (len inspection-type) u0) (err u203))
    
    (map-set inspections
      { inspection-id: inspection-id }
      {
        facility-id: facility-id,
        inspector: tx-sender,
        inspection-date: timestamp,
        inspection-type: inspection-type,
        overall-score: overall-score,
        status: (if (>= overall-score u70) "PASS" "FAIL"),
        notes: notes
      }
    )
    
    (var-set next-inspection-id (+ inspection-id u1))
    (var-set current-timestamp (+ timestamp u1))
    (ok inspection-id)
  )
)

;; Record a violation during inspection
(define-public (record-violation (inspection-id uint)
                                (violation-type (string-ascii 100))
                                (severity (string-ascii 20))
                                (description (string-ascii 300)))
  (let (
    (violation-id (var-get next-violation-id))
    (inspection-data (map-get? inspections { inspection-id: inspection-id }))
  )
    (asserts! (is-some inspection-data) (err u300))
    (asserts! (is-eq tx-sender (get inspector (unwrap-panic inspection-data))) (err u301))
    (asserts! (> (len violation-type) u0) (err u302))
    (asserts! (or (is-eq severity "LOW") (or (is-eq severity "MEDIUM") (is-eq severity "HIGH"))) (err u303))
    
    (map-set violations
      { violation-id: violation-id }
      {
        inspection-id: inspection-id,
        violation-type: violation-type,
        severity: severity,
        description: description,
        is-corrected: false,
        correction-date: none
      }
    )
    
    (var-set next-violation-id (+ violation-id u1))
    (ok violation-id)
  )
)

;; Mark violation as corrected
(define-public (mark-violation-corrected (violation-id uint))
  (let (
    (violation-data (map-get? violations { violation-id: violation-id }))
    (timestamp (var-get current-timestamp))
  )
    (asserts! (is-some violation-data) (err u400))
    (let ((violation (unwrap-panic violation-data)))
      (asserts! (not (get is-corrected violation)) (err u401))
      
      (map-set violations
        { violation-id: violation-id }
        (merge violation {
          is-corrected: true,
          correction-date: (some timestamp)
        })
      )
      (var-set current-timestamp (+ timestamp u1))
      (ok true)
    )
  )
)

;; Deactivate a facility (only contract owner)
(define-public (deactivate-facility (facility-id uint))
  (let (
    (facility-data (map-get? facilities { facility-id: facility-id }))
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u100))
    (asserts! (is-some facility-data) (err u500))
    
    (map-set facilities
      { facility-id: facility-id }
      (merge (unwrap-panic facility-data) { is-active: false })
    )
    (ok true)
  )
)

;; Read-only Functions

;; Get facility information
(define-read-only (get-facility (facility-id uint))
  (map-get? facilities { facility-id: facility-id })
)

;; Get inspection details
(define-read-only (get-inspection (inspection-id uint))
  (map-get? inspections { inspection-id: inspection-id })
)

;; Get violation details
(define-read-only (get-violation (violation-id uint))
  (map-get? violations { violation-id: violation-id })
)

;; Check if inspector is authorized
(define-read-only (is-authorized-inspector (inspector principal))
  (default-to false
    (get authorized (map-get? facility-inspectors { inspector: inspector }))
  )
)

;; Get facility inspection status (public reporting)
(define-read-only (get-facility-status (facility-id uint))
  (let (
    (facility (map-get? facilities { facility-id: facility-id }))
  )
    (match facility
      facility-data
      (ok {
        name: (get name facility-data),
        address: (get address facility-data),
        facility-type: (get facility-type facility-data),
        is-active: (get is-active facility-data)
      })
      (err u404)
    )
  )
)

;; Get next IDs (useful for frontend integration)
(define-read-only (get-next-facility-id)
  (var-get next-facility-id)
)

(define-read-only (get-next-inspection-id)
  (var-get next-inspection-id)
)

(define-read-only (get-next-violation-id)
  (var-get next-violation-id)
)

;; Get contract owner
(define-read-only (get-contract-owner)
  (var-get contract-owner)
)


;; title: health-inspector
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

