;; ===================================================================================
;; DECENTRALIZED COMMUNITY GREYWATER RECYCLING SYSTEM
;; ===================================================================================
;; A comprehensive platform for managing community water reuse with filtration
;; system maintenance, usage optimization, and environmental impact tracking.
;; Includes education, system design sharing, and community resilience building.
;; ===================================================================================

;; ===================================================================================
;; CONSTANTS AND ERROR CODES
;; ===================================================================================

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-PARAMS (err u101))
(define-constant ERR-SYSTEM-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))
(define-constant ERR-MAINTENANCE-OVERDUE (err u104))
(define-constant ERR-ALREADY-EXISTS (err u105))
(define-constant ERR-INVALID-STATUS (err u106))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u107))

;; System status constants
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-MAINTENANCE u2)
(define-constant STATUS-OFFLINE u3)
(define-constant STATUS-PENDING u4)

;; Maintenance types
(define-constant MAINTENANCE-FILTER u1)
(define-constant MAINTENANCE-PUMP u2)
(define-constant MAINTENANCE-SENSOR u3)
(define-constant MAINTENANCE-FULL-SERVICE u4)

;; Water quality levels
(define-constant QUALITY-EXCELLENT u5)
(define-constant QUALITY-GOOD u4)
(define-constant QUALITY-FAIR u3)
(define-constant QUALITY-POOR u2)
(define-constant QUALITY-UNSAFE u1)

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Minimum reputation for system operations
(define-constant MIN-REPUTATION u50)

;; ===================================================================================
;; DATA STRUCTURES
;; ===================================================================================

;; Community greywater recycling system
(define-map greywater-systems
  { system-id: uint }
  {
    owner: principal,
    location: (string-ascii 100),
    capacity-liters: uint,
    installation-date: uint,
    last-maintenance: uint,
    next-maintenance-due: uint,
    status: uint,
    total-water-processed: uint,
    current-water-level: uint,
    water-quality-score: uint,
    system-type: (string-ascii 50),
    filtration-stages: uint,
    energy-consumption-kwh: uint,
    maintenance-cost-total: uint,
    community-rating: uint,
    design-shared: bool
  }
)

;; System maintenance records
(define-map maintenance-records
  { system-id: uint, maintenance-id: uint }
  {
    technician: principal,
    maintenance-type: uint,
    date-performed: uint,
    cost: uint,
    duration-hours: uint,
    parts-replaced: (list 10 (string-ascii 50)),
    quality-before: uint,
    quality-after: uint,
    notes: (string-ascii 500),
    verified: bool
  }
)

;; Water usage tracking
(define-map usage-records
  { system-id: uint, date: uint }
  {
    water-input-liters: uint,
    water-output-liters: uint,
    efficiency-percentage: uint,
    households-served: uint,
    cost-savings-cents: uint,
    environmental-impact-score: uint
  }
)

;; Community member profiles
(define-map community-members
  { member: principal }
  {
    reputation-score: uint,
    systems-owned: uint,
    maintenance-performed: uint,
    water-saved-total: uint,
    contributions: uint,
    education-level: uint,
    joined-date: uint,
    active-participant: bool
  }
)

;; System design templates
(define-map system-designs
  { design-id: uint }
  {
    creator: principal,
    name: (string-ascii 100),
    description: (string-ascii 500),
    capacity-range: { min: uint, max: uint },
    estimated-cost: uint,
    complexity-rating: uint,
    filtration-method: (string-ascii 100),
    components: (list 20 (string-ascii 50)),
    efficiency-rating: uint,
    downloads: uint,
    rating: uint,
    open-source: bool
  }
)

;; Educational content
(define-map educational-content
  { content-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    content-type: (string-ascii 20),
    difficulty-level: uint,
    views: uint,
    rating: uint,
    topics: (list 10 (string-ascii 30)),
    created-date: uint,
    verified: bool
  }
)

;; Emergency drought response
(define-map drought-response
  { region-id: uint }
  {
    alert-level: uint,
    water-restrictions: bool,
    priority-systems: (list 50 uint),
    community-coordinator: principal,
    resources-available: uint,
    emergency-protocols: (list 10 (string-ascii 100)),
    last-updated: uint
  }
)

;; ===================================================================================
;; DATA VARIABLES
;; ===================================================================================

(define-data-var next-system-id uint u1)
(define-data-var next-maintenance-id uint u1)
(define-data-var next-design-id uint u1)
(define-data-var next-content-id uint u1)
(define-data-var total-water-saved uint u0)
(define-data-var total-systems uint u0)
(define-data-var platform-fee-percentage uint u2) ;; 2% platform fee
(define-data-var emergency-mode bool false)

;; ===================================================================================
;; SYSTEM REGISTRATION AND MANAGEMENT
;; ===================================================================================

;; Register a new greywater recycling system
(define-public (register-system
    (location (string-ascii 100))
    (capacity-liters uint)
    (system-type (string-ascii 50))
    (filtration-stages uint))
  (let ((system-id (var-get next-system-id)))
    (begin
      (asserts! (> capacity-liters u0) ERR-INVALID-PARAMS)
      (asserts! (> filtration-stages u0) ERR-INVALID-PARAMS)

      ;; Initialize community member if new
      (if (is-none (map-get? community-members { member: tx-sender }))
        (map-set community-members
          { member: tx-sender }
          {
            reputation-score: u100,
            systems-owned: u1,
            maintenance-performed: u0,
            water-saved-total: u0,
            contributions: u0,
            education-level: u1,
            joined-date: u1,
            active-participant: true
          })
        (map-set community-members
          { member: tx-sender }
          (merge (unwrap-panic (map-get? community-members { member: tx-sender }))
                 { systems-owned: (+ (get systems-owned (unwrap-panic (map-get? community-members { member: tx-sender }))) u1) })))

      ;; Register the system
      (map-set greywater-systems
        { system-id: system-id }
        {
          owner: tx-sender,
          location: location,
          capacity-liters: capacity-liters,
          installation-date: u1,
          last-maintenance: u1,
          next-maintenance-due: u2161, ;; ~30 days from now
          status: STATUS-PENDING,
          total-water-processed: u0,
          current-water-level: u0,
          water-quality-score: u0,
          system-type: system-type,
          filtration-stages: filtration-stages,
          energy-consumption-kwh: u0,
          maintenance-cost-total: u0,
          community-rating: u0,
          design-shared: false
        })

      (var-set next-system-id (+ system-id u1))
      (var-set total-systems (+ (var-get total-systems) u1))
      (ok system-id))))

;; Update system status
(define-public (update-system-status (system-id uint) (new-status uint))
  (let ((system (unwrap! (map-get? greywater-systems { system-id: system-id }) ERR-SYSTEM-NOT-FOUND)))
    (asserts! (is-eq (get owner system) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-status STATUS-PENDING) ERR-INVALID-STATUS)

    (map-set greywater-systems
      { system-id: system-id }
      (merge system { status: new-status }))
    (ok true)))

;; ===================================================================================
;; WATER USAGE TRACKING AND OPTIMIZATION
;; ===================================================================================

;; Record daily water usage
(define-public (record-usage
    (system-id uint)
    (water-input-liters uint)
    (water-output-liters uint)
    (households-served uint))
  (let ((system (unwrap! (map-get? greywater-systems { system-id: system-id }) ERR-SYSTEM-NOT-FOUND))
        (efficiency (if (> water-input-liters u0)
                       (/ (* water-output-liters u100) water-input-liters)
                       u0))
        (cost-savings (* water-output-liters u5)) ;; Assuming 5 cents per liter saved
        (impact-score (+ efficiency (/ households-served u2))))

    (begin
      (asserts! (is-eq (get owner system) tx-sender) ERR-NOT-AUTHORIZED)
      (asserts! (is-eq (get status system) STATUS-ACTIVE) ERR-INVALID-STATUS)

      ;; Record usage
      (map-set usage-records
        { system-id: system-id, date: u1 }
        {
          water-input-liters: water-input-liters,
          water-output-liters: water-output-liters,
          efficiency-percentage: efficiency,
          households-served: households-served,
          cost-savings-cents: cost-savings,
          environmental-impact-score: impact-score
        })

      ;; Update system totals
      (map-set greywater-systems
        { system-id: system-id }
        (merge system
          {
            total-water-processed: (+ (get total-water-processed system) water-input-liters),
            current-water-level: water-output-liters
          }))

      ;; Update global water saved
      (var-set total-water-saved (+ (var-get total-water-saved) water-output-liters))

      ;; Update member contribution
      (let ((member-data (unwrap-panic (map-get? community-members { member: tx-sender }))))
        (map-set community-members
          { member: tx-sender }
          (merge member-data
            {
              water-saved-total: (+ (get water-saved-total member-data) water-output-liters),
              contributions: (+ (get contributions member-data) u1)
            })))

      (ok true))))

;; Get usage optimization recommendations
(define-read-only (get-optimization-recommendations (system-id uint))
  (let ((system (unwrap! (map-get? greywater-systems { system-id: system-id }) ERR-SYSTEM-NOT-FOUND)))
    (ok {
      efficiency-target: u85,
      current-efficiency: (if (> (get total-water-processed system) u0)
                            (/ (* (get current-water-level system) u100) (get total-water-processed system))
                            u0),
      maintenance-due: (< u1 (get next-maintenance-due system)),
      recommended-actions: (list "Check filter cleanliness" "Optimize water input timing" "Verify sensor calibration")
    })))

;; ===================================================================================
;; MAINTENANCE MANAGEMENT
;; ===================================================================================

;; Schedule maintenance
(define-public (schedule-maintenance
    (system-id uint)
    (maintenance-type uint)
    (technician principal))
  (let ((system (unwrap! (map-get? greywater-systems { system-id: system-id }) ERR-SYSTEM-NOT-FOUND))
        (maintenance-id (var-get next-maintenance-id)))

    (asserts! (is-eq (get owner system) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (<= maintenance-type MAINTENANCE-FULL-SERVICE) ERR-INVALID-PARAMS)

    ;; Check technician reputation
    (let ((tech-profile (default-to
           { reputation-score: u0, systems-owned: u0, maintenance-performed: u0,
             water-saved-total: u0, contributions: u0, education-level: u0,
             joined-date: u0, active-participant: false }
           (map-get? community-members { member: technician }))))
      (asserts! (>= (get reputation-score tech-profile) MIN-REPUTATION) ERR-INSUFFICIENT-REPUTATION))

    ;; Update system status to maintenance
    (map-set greywater-systems
      { system-id: system-id }
      (merge system { status: STATUS-MAINTENANCE }))

    (var-set next-maintenance-id (+ maintenance-id u1))
    (ok maintenance-id)))

;; Complete maintenance
(define-public (complete-maintenance
    (system-id uint)
    (maintenance-id uint)
    (cost uint)
    (duration-hours uint)
    (parts-replaced (list 10 (string-ascii 50)))
    (quality-after uint))
  (let ((system (unwrap! (map-get? greywater-systems { system-id: system-id }) ERR-SYSTEM-NOT-FOUND)))

    ;; Record maintenance
    (map-set maintenance-records
      { system-id: system-id, maintenance-id: maintenance-id }
      {
        technician: tx-sender,
        maintenance-type: MAINTENANCE-FILTER, ;; Default type
        date-performed: u1,
        cost: cost,
        duration-hours: duration-hours,
        parts-replaced: parts-replaced,
        quality-before: (get water-quality-score system),
        quality-after: quality-after,
        notes: "Maintenance completed successfully",
        verified: false
      })

    ;; Update system
    (map-set greywater-systems
      { system-id: system-id }
      (merge system
        {
          status: STATUS-ACTIVE,
          last-maintenance: u1,
          next-maintenance-due: u2161, ;; Next maintenance in ~30 days
          water-quality-score: quality-after,
          maintenance-cost-total: (+ (get maintenance-cost-total system) cost)
        }))

    ;; Update technician reputation
    (let ((tech-profile (unwrap-panic (map-get? community-members { member: tx-sender }))))
      (map-set community-members
        { member: tx-sender }
        (merge tech-profile
          {
            maintenance-performed: (+ (get maintenance-performed tech-profile) u1),
            reputation-score: (+ (get reputation-score tech-profile) u5)
          })))

    (ok true)))

;; ===================================================================================
;; ENVIRONMENTAL IMPACT TRACKING
;; ===================================================================================

;; Get environmental impact report
(define-read-only (get-environmental-impact (system-id uint))
  (let ((system (unwrap! (map-get? greywater-systems { system-id: system-id }) ERR-SYSTEM-NOT-FOUND)))
    (ok {
      total-water-recycled: (get total-water-processed system),
      carbon-footprint-reduced: (* (get total-water-processed system) u2), ;; kg CO2 equivalent
      energy-efficiency: (if (> (get energy-consumption-kwh system) u0)
                           (/ (get total-water-processed system) (get energy-consumption-kwh system))
                           u0),
      ecosystem-benefit-score: (+ (get water-quality-score system)
                                 (/ (get total-water-processed system) u1000))
    })))

;; ===================================================================================
;; COMMUNITY FEATURES
;; ===================================================================================

;; Share system design
(define-public (share-system-design
    (name (string-ascii 100))
    (description (string-ascii 500))
    (capacity-range-min uint)
    (capacity-range-max uint)
    (estimated-cost uint)
    (complexity-rating uint)
    (filtration-method (string-ascii 100))
    (components (list 20 (string-ascii 50)))
    (open-source bool))
  (let ((design-id (var-get next-design-id))
        (member-profile (unwrap! (map-get? community-members { member: tx-sender }) ERR-NOT-AUTHORIZED)))

    (asserts! (>= (get reputation-score member-profile) MIN-REPUTATION) ERR-INSUFFICIENT-REPUTATION)
    (asserts! (> capacity-range-max capacity-range-min) ERR-INVALID-PARAMS)

    (map-set system-designs
      { design-id: design-id }
      {
        creator: tx-sender,
        name: name,
        description: description,
        capacity-range: { min: capacity-range-min, max: capacity-range-max },
        estimated-cost: estimated-cost,
        complexity-rating: complexity-rating,
        filtration-method: filtration-method,
        components: components,
        efficiency-rating: u0,
        downloads: u0,
        rating: u0,
        open-source: open-source
      })

    ;; Update member contributions
    (map-set community-members
      { member: tx-sender }
      (merge member-profile
        {
          contributions: (+ (get contributions member-profile) u5),
          reputation-score: (+ (get reputation-score member-profile) u10)
        }))

    (var-set next-design-id (+ design-id u1))
    (ok design-id)))

;; Create educational content
(define-public (create-educational-content
    (title (string-ascii 100))
    (content-type (string-ascii 20))
    (difficulty-level uint)
    (topics (list 10 (string-ascii 30))))
  (let ((content-id (var-get next-content-id))
        (member-profile (unwrap! (map-get? community-members { member: tx-sender }) ERR-NOT-AUTHORIZED)))

    (asserts! (>= (get reputation-score member-profile) MIN-REPUTATION) ERR-INSUFFICIENT-REPUTATION)
    (asserts! (<= difficulty-level u5) ERR-INVALID-PARAMS)

    (map-set educational-content
      { content-id: content-id }
      {
        creator: tx-sender,
        title: title,
        content-type: content-type,
        difficulty-level: difficulty-level,
        views: u0,
        rating: u0,
        topics: topics,
        created-date: u1,
        verified: false
      })

    ;; Update member education contribution
    (map-set community-members
      { member: tx-sender }
      (merge member-profile
        {
          contributions: (+ (get contributions member-profile) u3),
          education-level: (+ (get education-level member-profile) u1)
        }))

    (var-set next-content-id (+ content-id u1))
    (ok content-id)))

;; ===================================================================================
;; DROUGHT RESPONSE AND RESILIENCE
;; ===================================================================================

;; Activate drought emergency mode
(define-public (activate-drought-response
    (region-id uint)
    (alert-level uint)
    (priority-systems (list 50 uint)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= alert-level u5) ERR-INVALID-PARAMS)

    (map-set drought-response
      { region-id: region-id }
      {
        alert-level: alert-level,
        water-restrictions: (>= alert-level u3),
        priority-systems: priority-systems,
        community-coordinator: tx-sender,
        resources-available: u1000000, ;; Emergency fund in microSTX
        emergency-protocols: (list "Prioritize drinking water" "Reduce irrigation" "Increase recycling efficiency"),
        last-updated: u1
      })

    (if (>= alert-level u4)
      (var-set emergency-mode true)
      (var-set emergency-mode false))

    (ok true)))

;; ===================================================================================
;; READ-ONLY FUNCTIONS
;; ===================================================================================

;; Get system information
(define-read-only (get-system-info (system-id uint))
  (map-get? greywater-systems { system-id: system-id }))

;; Get community member profile
(define-read-only (get-member-profile (member principal))
  (map-get? community-members { member: member }))

;; Get system design
(define-read-only (get-system-design (design-id uint))
  (map-get? system-designs { design-id: design-id }))

;; Get community statistics
(define-read-only (get-community-stats)
  (ok {
    total-systems: (var-get total-systems),
    total-water-saved: (var-get total-water-saved),
    active-designs: (var-get next-design-id),
    educational-content: (var-get next-content-id),
    emergency-mode: (var-get emergency-mode)
  }))

;; Get maintenance history
(define-read-only (get-maintenance-history (system-id uint))
  (ok (map-get? maintenance-records { system-id: system-id, maintenance-id: u1 }))) ;; Simplified for demo

;; Check if maintenance is overdue
(define-read-only (is-maintenance-overdue (system-id uint))
  (let ((system (unwrap! (map-get? greywater-systems { system-id: system-id }) ERR-SYSTEM-NOT-FOUND)))
    (ok (> u1 (get next-maintenance-due system)))))

;; ===================================================================================
;; ADMIN FUNCTIONS
;; ===================================================================================

;; Update platform fee (admin only)
(define-public (update-platform-fee (new-fee-percentage uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-fee-percentage u10) ERR-INVALID-PARAMS) ;; Max 10%
    (var-set platform-fee-percentage new-fee-percentage)
    (ok true)))

;; Emergency shutdown (admin only)
(define-public (emergency-shutdown)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set emergency-mode true)
    (ok true)))
