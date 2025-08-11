;; contracts/pet-registry.clar
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-ALREADY-EXISTS (err u409))

(define-data-var next-pet-id uint u1)

(define-map pets
  uint
  {
    owner: principal,
    name: (string-ascii 50),
    description: (string-ascii 500),
    photo-hash: (string-ascii 64),
    status: (string-ascii 20),
    last-seen-lat: int,
    last-seen-lon: int,
    created-at: uint,
    updated-at: uint
  })

(define-map pet-owner-ids principal (list 100 uint))

(define-public (register-missing-pet (name (string-ascii 50))
                                   (description (string-ascii 500))
                                   (photo-hash (string-ascii 64))
                                   (lat int)
                                   (lon int))
  (let ((pet-id (var-get next-pet-id))
        (current-height block-height))
    (map-set pets pet-id {
      owner: tx-sender,
      name: name,
      description: description,
      photo-hash: photo-hash,
      status: "missing",
      last-seen-lat: lat,
      last-seen-lon: lon,
      created-at: current-height,
      updated-at: current-height
    })
    (var-set next-pet-id (+ pet-id u1))
    (map-set pet-owner-ids tx-sender
      (unwrap-panic (as-max-len?
        (append (default-to (list) (map-get? pet-owner-ids tx-sender)) pet-id)
        u100)))
    (ok pet-id)))

(define-public (update-pet-status (pet-id uint) (new-status (string-ascii 20)))
  (let ((pet-data (unwrap! (map-get? pets pet-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner pet-data)) ERR-UNAUTHORIZED)
    (map-set pets pet-id (merge pet-data {
      status: new-status,
      updated-at: block-height
    }))
    (ok true)))

(define-public (update-location (pet-id uint) (lat int) (lon int))
  (let ((pet-data (unwrap! (map-get? pets pet-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner pet-data)) ERR-UNAUTHORIZED)
    (map-set pets pet-id (merge pet-data {
      last-seen-lat: lat,
      last-seen-lon: lon,
      updated-at: block-height
    }))
    (ok true)))

(define-read-only (get-pet (pet-id uint))
  (map-get? pets pet-id))

(define-read-only (get-owner-pets (owner principal))
  (map-get? pet-owner-ids owner))

(define-read-only (is-pet-owner (pet-id uint) (owner principal))
  (match (map-get? pets pet-id)
    pet-data (is-eq owner (get owner pet-data))
    false))

;; contracts/sighting-reports.clar
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-UNAUTHORIZED (err u401))

(define-data-var next-sighting-id uint u1)

(define-map sightings
  uint
  {
    reporter: principal,
    pet-id: uint,
    lat: int,
    lon: int,
    description: (string-ascii 500),
    photo-hash: (optional (string-ascii 64)),
    created-at: uint,
    verified: bool
  })

(define-map pet-sightings uint (list 50 uint))

(define-public (report-sighting (pet-id uint)
                               (lat int)
                               (lon int)
                               (description (string-ascii 500))
                               (photo-hash (optional (string-ascii 64))))
  (let ((sighting-id (var-get next-sighting-id))
        (current-height block-height))
    (map-set sightings sighting-id {
      reporter: tx-sender,
      pet-id: pet-id,
      lat: lat,
      lon: lon,
      description: description,
      photo-hash: photo-hash,
      created-at: current-height,
      verified: false
    })
    (var-set next-sighting-id (+ sighting-id u1))
    (map-set pet-sightings pet-id
      (unwrap-panic (as-max-len?
        (append (default-to (list) (map-get? pet-sightings pet-id)) sighting-id)
        u50)))
    (ok sighting-id)))

(define-public (verify-sighting (sighting-id uint) (pet-id uint))
  (let ((sighting-data (unwrap! (map-get? sightings sighting-id) ERR-NOT-FOUND)))
    (asserts! (contract-call? .pet-registry is-pet-owner pet-id tx-sender) ERR-UNAUTHORIZED)
    (map-set sightings sighting-id (merge sighting-data {
      verified: true
    }))
    (ok true)))

(define-read-only (get-sighting (sighting-id uint))
  (map-get? sightings sighting-id))

(define-read-only (get-pet-sightings (pet-id uint))
  (map-get? pet-sightings pet-id))
