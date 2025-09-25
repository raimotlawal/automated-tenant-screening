;; Tenant Screening System Contract
;; Verify tenant credentials, track rental history, and maintain privacy-compliant records

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-UNAUTHORIZED (err u102))
(define-constant ERR-INVALID-SCORE (err u103))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u104))
(define-constant ERR-ALREADY-EXISTS (err u105))
(define-constant ERR-INVALID-STATUS (err u106))
(define-constant ERR-PRIVACY-VIOLATION (err u107))
(define-constant ERR-CONSENT-REQUIRED (err u108))
(define-constant ERR-DATA-EXPIRED (err u109))

;; Application Status
(define-constant STATUS-PENDING u0)
(define-constant STATUS-IN-REVIEW u1)
(define-constant STATUS-APPROVED u2)
(define-constant STATUS-REJECTED u3)
(define-constant STATUS-EXPIRED u4)

;; Credit Score Ranges
(define-constant CREDIT-EXCELLENT u800)
(define-constant CREDIT-GOOD u700)
(define-constant CREDIT-FAIR u600)
(define-constant CREDIT-POOR u500)

;; Screening Fees
(define-constant SCREENING-FEE u1000000) ;; 1 STX in microSTX
(define-constant PREMIUM-SCREENING-FEE u2000000) ;; 2 STX for comprehensive screening
(define-constant VERIFICATION-REWARD u50000) ;; 0.05 STX for data providers

;; Risk Assessment Scores
(define-constant RISK-LOW u1)
(define-constant RISK-MEDIUM u2)
(define-constant RISK-HIGH u3)
(define-constant RISK-CRITICAL u4)

;; Data Variables
(define-data-var next-tenant-id uint u1)
(define-data-var next-application-id uint u1)
(define-data-var next-screening-id uint u1)
(define-data-var total-screenings uint u0)
(define-data-var platform-revenue uint u0)

;; Tenant Registry - Verified tenant profiles
(define-map tenant-registry
  { tenant-id: uint }
  {
    tenant-address: principal,
    profile-hash: (string-ascii 64), ;; IPFS hash of encrypted profile
    verification-level: uint,
    registration-time: uint,
    last-updated: uint,
    consent-granted: bool,
    privacy-settings: uint,
    total-applications: uint
  }
)

;; Rental Applications - Individual screening requests
(define-map rental-applications
  { application-id: uint }
  {
    tenant-id: uint,
    landlord: principal,
    property-hash: (string-ascii 64), ;; IPFS hash of property details
    application-time: uint,
    status: uint,
    screening-fee-paid: bool,
    consent-granted: bool,
    privacy-level: uint,
    expiry-time: uint
  }
)

;; Screening Reports - Comprehensive tenant assessments
(define-map screening-reports
  { screening-id: uint }
  {
    application-id: uint,
    tenant-id: uint,
    credit-score: uint,
    income-verification: bool,
    employment-verified: bool,
    rental-history-score: uint,
    risk-assessment: uint,
    recommendation: bool,
    report-hash: (string-ascii 64), ;; IPFS hash of detailed report
    screening-time: uint,
    verified-by: principal
  }
)

;; Rental History Records - Immutable rental track record
(define-map rental-history
  { tenant-id: uint, history-id: uint }
  {
    property-address: (string-ascii 200),
    landlord: principal,
    lease-start: uint,
    lease-end: uint,
    monthly-rent: uint,
    payment-history-score: uint,
    property-condition-score: uint,
    landlord-rating: uint,
    verified: bool,
    verification-time: uint
  }
)

;; Credit Verification Records - Financial assessment data
(define-map credit-verifications
  { tenant-id: uint }
  {
    credit-score: uint,
    debt-to-income-ratio: uint,
    employment-status: uint,
    monthly-income: uint,
    verification-source: principal,
    verification-time: uint,
    validity-period: uint,
    consent-granted: bool
  }
)

;; Landlord Registry - Verified property owners
(define-map landlord-registry
  { landlord: principal }
  {
    verification-status: bool,
    total-properties: uint,
    total-screenings: uint,
    reputation-score: uint,
    registration-time: uint,
    contact-hash: (string-ascii 64) ;; IPFS hash of contact details
  }
)

;; Privacy Consents - Granular permission tracking
(define-map privacy-consents
  { tenant-id: uint, data-requester: principal }
  {
    consent-granted: bool,
    consent-time: uint,
    data-types-allowed: uint,
    expiry-time: uint,
    revoked: bool
  }
)

;; Verification Providers - Trusted data sources
(define-map verification-providers
  { provider: principal }
  {
    verification-type: uint,
    reputation-score: uint,
    total-verifications: uint,
    accuracy-rate: uint,
    registration-time: uint,
    active: bool
  }
)

;; Screening Statistics - Platform analytics
(define-map screening-stats
  { stat-type: uint }
  { value: uint, last-updated: uint }
)

;; Read-only functions

;; Get tenant profile
(define-read-only (get-tenant-profile (tenant-id uint))
  (map-get? tenant-registry { tenant-id: tenant-id })
)

;; Get application details
(define-read-only (get-application (application-id uint))
  (map-get? rental-applications { application-id: application-id })
)

;; Get screening report
(define-read-only (get-screening-report (screening-id uint))
  (map-get? screening-reports { screening-id: screening-id })
)

;; Get rental history
(define-read-only (get-rental-history (tenant-id uint) (history-id uint))
  (map-get? rental-history { tenant-id: tenant-id, history-id: history-id })
)

;; Get credit verification
(define-read-only (get-credit-verification (tenant-id uint))
  (map-get? credit-verifications { tenant-id: tenant-id })
)

;; Get landlord profile
(define-read-only (get-landlord-profile (landlord principal))
  (map-get? landlord-registry { landlord: landlord })
)

;; Check privacy consent
(define-read-only (has-privacy-consent (tenant-id uint) (requester principal))
  (match (map-get? privacy-consents { tenant-id: tenant-id, data-requester: requester })
    consent (and 
      (get consent-granted consent)
      (not (get revoked consent))
      (< block-height (get expiry-time consent))
    )
    false
  )
)

;; Calculate risk score based on multiple factors
(define-read-only (calculate-risk-score (credit-score uint) (income-ratio uint) (history-score uint))
  (let (
    (credit-risk (if (>= credit-score CREDIT-GOOD) u1 
                   (if (>= credit-score CREDIT-FAIR) u2 u3)))
    (income-risk (if (>= income-ratio u300) u1 
                   (if (>= income-ratio u250) u2 u3)))
    (history-risk (if (>= history-score u80) u1 
                    (if (>= history-score u60) u2 u3)))
    (total-risk (+ credit-risk income-risk history-risk))
  )
    (if (<= total-risk u4) RISK-LOW
      (if (<= total-risk u6) RISK-MEDIUM
        (if (<= total-risk u8) RISK-HIGH RISK-CRITICAL)
      )
    )
  )
)

;; Get platform statistics
(define-read-only (get-platform-stats)
  {
    total-tenants: (- (var-get next-tenant-id) u1),
    total-applications: (- (var-get next-application-id) u1),
    total-screenings: (var-get total-screenings),
    platform-revenue: (var-get platform-revenue)
  }
)

;; Public functions

;; Register tenant profile
(define-public (register-tenant
    (profile-hash (string-ascii 64))
    (privacy-settings uint)
  )
  (let (
    (tenant-id (var-get next-tenant-id))
    (current-time block-height)
  )
    ;; Check if tenant already registered
    (asserts! (is-none (map-get? tenant-registry { tenant-id: tenant-id })) ERR-ALREADY-EXISTS)
    
    ;; Register tenant
    (map-set tenant-registry
      { tenant-id: tenant-id }
      {
        tenant-address: tx-sender,
        profile-hash: profile-hash,
        verification-level: u1, ;; Basic verification
        registration-time: current-time,
        last-updated: current-time,
        consent-granted: true,
        privacy-settings: privacy-settings,
        total-applications: u0
      }
    )
    
    ;; Update counter
    (var-set next-tenant-id (+ tenant-id u1))
    
    (ok tenant-id)
  )
)

;; Register landlord
(define-public (register-landlord (contact-hash (string-ascii 64)))
  (let (
    (current-time block-height)
  )
    ;; Check if landlord already registered
    (asserts! (is-none (map-get? landlord-registry { landlord: tx-sender })) ERR-ALREADY-EXISTS)
    
    ;; Register landlord
    (map-set landlord-registry
      { landlord: tx-sender }
      {
        verification-status: false, ;; Requires verification
        total-properties: u0,
        total-screenings: u0,
        reputation-score: u100, ;; Starting reputation
        registration-time: current-time,
        contact-hash: contact-hash
      }
    )
    
    (ok true)
  )
)

;; Submit rental application
(define-public (submit-application
    (tenant-id uint)
    (property-hash (string-ascii 64))
    (privacy-level uint)
  )
  (let (
    (application-id (var-get next-application-id))
    (tenant (unwrap! (map-get? tenant-registry { tenant-id: tenant-id }) ERR-NOT-FOUND))
    (landlord-profile (unwrap! (map-get? landlord-registry { landlord: tx-sender }) ERR-UNAUTHORIZED))
    (current-time block-height)
    (expiry-time (+ current-time u1008)) ;; ~1 week expiry
  )
    ;; Validate tenant ownership or consent
    (asserts! (or 
      (is-eq (get tenant-address tenant) tx-sender)
      (has-privacy-consent tenant-id tx-sender)
    ) ERR-UNAUTHORIZED)
    
    ;; Validate payment
    (asserts! (>= (stx-get-balance tx-sender) SCREENING-FEE) ERR-INSUFFICIENT-PAYMENT)
    
    ;; Pay screening fee
    (try! (stx-transfer? SCREENING-FEE tx-sender CONTRACT-OWNER))
    
    ;; Create application
    (map-set rental-applications
      { application-id: application-id }
      {
        tenant-id: tenant-id,
        landlord: tx-sender,
        property-hash: property-hash,
        application-time: current-time,
        status: STATUS-PENDING,
        screening-fee-paid: true,
        consent-granted: true,
        privacy-level: privacy-level,
        expiry-time: expiry-time
      }
    )
    
    ;; Update tenant application count
    (map-set tenant-registry
      { tenant-id: tenant-id }
      (merge tenant { 
        total-applications: (+ (get total-applications tenant) u1),
        last-updated: current-time
      })
    )
    
    ;; Update landlord screening count
    (map-set landlord-registry
      { landlord: tx-sender }
      (merge landlord-profile { 
        total-screenings: (+ (get total-screenings landlord-profile) u1)
      })
    )
    
    ;; Update platform revenue
    (var-set platform-revenue (+ (var-get platform-revenue) SCREENING-FEE))
    
    ;; Update counter
    (var-set next-application-id (+ application-id u1))
    
    (ok application-id)
  )
)

;; Submit credit verification
(define-public (submit-credit-verification
    (tenant-id uint)
    (credit-score uint)
    (debt-income-ratio uint)
    (employment-status uint)
    (monthly-income uint)
    (validity-period uint)
  )
  (let (
    (tenant (unwrap! (map-get? tenant-registry { tenant-id: tenant-id }) ERR-NOT-FOUND))
    (provider (unwrap! (map-get? verification-providers { provider: tx-sender }) ERR-UNAUTHORIZED))
    (current-time block-height)
  )
    ;; Validate provider is active and authorized
    (asserts! (get active provider) ERR-UNAUTHORIZED)
    (asserts! (and (>= credit-score u300) (<= credit-score u850)) ERR-INVALID-SCORE)
    
    ;; Submit credit verification
    (map-set credit-verifications
      { tenant-id: tenant-id }
      {
        credit-score: credit-score,
        debt-to-income-ratio: debt-income-ratio,
        employment-status: employment-status,
        monthly-income: monthly-income,
        verification-source: tx-sender,
        verification-time: current-time,
        validity-period: validity-period,
        consent-granted: true
      }
    )
    
    ;; Update provider statistics
    (map-set verification-providers
      { provider: tx-sender }
      (merge provider { 
        total-verifications: (+ (get total-verifications provider) u1)
      })
    )
    
    (ok true)
  )
)

;; Add rental history record
(define-public (add-rental-history
    (tenant-id uint)
    (history-id uint)
    (property-address (string-ascii 200))
    (lease-start uint)
    (lease-end uint)
    (monthly-rent uint)
    (payment-score uint)
    (condition-score uint)
    (rating uint)
  )
  (let (
    (tenant (unwrap! (map-get? tenant-registry { tenant-id: tenant-id }) ERR-NOT-FOUND))
    (landlord-profile (unwrap! (map-get? landlord-registry { landlord: tx-sender }) ERR-UNAUTHORIZED))
    (current-time block-height)
  )
    ;; Validate landlord is verified
    (asserts! (get verification-status landlord-profile) ERR-UNAUTHORIZED)
    (asserts! (and (>= payment-score u0) (<= payment-score u100)) ERR-INVALID-SCORE)
    (asserts! (and (>= condition-score u0) (<= condition-score u100)) ERR-INVALID-SCORE)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-SCORE)
    
    ;; Add rental history record
    (map-set rental-history
      { tenant-id: tenant-id, history-id: history-id }
      {
        property-address: property-address,
        landlord: tx-sender,
        lease-start: lease-start,
        lease-end: lease-end,
        monthly-rent: monthly-rent,
        payment-history-score: payment-score,
        property-condition-score: condition-score,
        landlord-rating: rating,
        verified: true,
        verification-time: current-time
      }
    )
    
    (ok true)
  )
)

;; Generate screening report
(define-public (generate-screening-report (application-id uint))
  (let (
    (application (unwrap! (map-get? rental-applications { application-id: application-id }) ERR-NOT-FOUND))
    (tenant-id (get tenant-id application))
    (credit-data (map-get? credit-verifications { tenant-id: tenant-id }))
    (screening-id (var-get next-screening-id))
    (current-time block-height)
  )
    ;; Validate requester is landlord
    (asserts! (is-eq tx-sender (get landlord application)) ERR-UNAUTHORIZED)
    (asserts! (get consent-granted application) ERR-CONSENT-REQUIRED)
    (asserts! (< current-time (get expiry-time application)) ERR-DATA-EXPIRED)
    
    ;; Calculate assessment scores
    (let (
      (credit-score (default-to u500 (get credit-score credit-data)))
      (income-ratio (default-to u200 (get debt-to-income-ratio credit-data)))
      (history-score u75) ;; Simplified calculation
      (risk-score (calculate-risk-score credit-score income-ratio history-score))
      (recommendation (and (>= credit-score CREDIT-FAIR) (<= risk-score RISK-MEDIUM)))
    )
      ;; Generate screening report
      (map-set screening-reports
        { screening-id: screening-id }
        {
          application-id: application-id,
          tenant-id: tenant-id,
          credit-score: credit-score,
          income-verification: (is-some credit-data),
          employment-verified: (is-some credit-data),
          rental-history-score: history-score,
          risk-assessment: risk-score,
          recommendation: recommendation,
          report-hash: "", ;; Would generate IPFS hash
          screening-time: current-time,
          verified-by: tx-sender
        }
      )
      
      ;; Update application status
      (map-set rental-applications
        { application-id: application-id }
        (merge application { 
          status: (if recommendation STATUS-APPROVED STATUS-REJECTED)
        })
      )
      
      ;; Update counters
      (var-set next-screening-id (+ screening-id u1))
      (var-set total-screenings (+ (var-get total-screenings) u1))
      
      (ok screening-id)
    )
  )
)

;; Grant privacy consent
(define-public (grant-privacy-consent
    (tenant-id uint)
    (data-requester principal)
    (data-types uint)
    (duration uint)
  )
  (let (
    (tenant (unwrap! (map-get? tenant-registry { tenant-id: tenant-id }) ERR-NOT-FOUND))
    (current-time block-height)
    (expiry-time (+ current-time duration))
  )
    ;; Validate tenant ownership
    (asserts! (is-eq tx-sender (get tenant-address tenant)) ERR-UNAUTHORIZED)
    
    ;; Grant consent
    (map-set privacy-consents
      { tenant-id: tenant-id, data-requester: data-requester }
      {
        consent-granted: true,
        consent-time: current-time,
        data-types-allowed: data-types,
        expiry-time: expiry-time,
        revoked: false
      }
    )
    
    (ok true)
  )
)

;; Revoke privacy consent
(define-public (revoke-privacy-consent (tenant-id uint) (data-requester principal))
  (let (
    (tenant (unwrap! (map-get? tenant-registry { tenant-id: tenant-id }) ERR-NOT-FOUND))
    (consent (unwrap! (map-get? privacy-consents 
      { tenant-id: tenant-id, data-requester: data-requester }) ERR-NOT-FOUND))
  )
    ;; Validate tenant ownership
    (asserts! (is-eq tx-sender (get tenant-address tenant)) ERR-UNAUTHORIZED)
    
    ;; Revoke consent
    (map-set privacy-consents
      { tenant-id: tenant-id, data-requester: data-requester }
      (merge consent { revoked: true })
    )
    
    (ok true)
  )
)

;; Register verification provider
(define-public (register-verification-provider (verification-type uint))
  (let (
    (current-time block-height)
  )
    ;; Check if provider already registered
    (asserts! (is-none (map-get? verification-providers { provider: tx-sender })) ERR-ALREADY-EXISTS)
    
    ;; Register provider
    (map-set verification-providers
      { provider: tx-sender }
      {
        verification-type: verification-type,
        reputation-score: u100,
        total-verifications: u0,
        accuracy-rate: u100,
        registration-time: current-time,
        active: false ;; Requires admin approval
      }
    )
    
    (ok true)
  )
)

;; Admin functions

;; Verify landlord (admin only)
(define-public (verify-landlord (landlord principal))
  (let (
    (landlord-profile (unwrap! (map-get? landlord-registry { landlord: landlord }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    
    (map-set landlord-registry
      { landlord: landlord }
      (merge landlord-profile { verification-status: true })
    )
    
    (ok true)
  )
)

;; Approve verification provider (admin only)
(define-public (approve-verification-provider (provider principal))
  (let (
    (provider-profile (unwrap! (map-get? verification-providers { provider: provider }) ERR-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    
    (map-set verification-providers
      { provider: provider }
      (merge provider-profile { active: true })
    )
    
    (ok true)
  )
)


;; title: tenant-screening-system
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

