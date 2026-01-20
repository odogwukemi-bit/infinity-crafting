;; Infinity Crafting - Core Smart Contract
;; A blockchain-based crafting game with NFT items and community-driven recipe discovery

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-invalid-recipe (err u104))
(define-constant err-unauthorized (err u105))

;; Data Variables
(define-data-var recipe-nonce uint u0)
(define-data-var item-nonce uint u0)
(define-data-var validation-threshold uint u3) ;; Votes needed to validate recipe

;; NFT Definition for Crafted Items
(define-non-fungible-token crafted-item uint)

;; Fungible Token for Governance
(define-fungible-token governance-token)

;; Data Maps
(define-map items
  uint
  {
    name: (string-ascii 50),
    tier: uint, ;; 1=base, 2=intermediate, 3=legendary
    creator: principal,
    recipe-id: uint
  }
)

(define-map recipes
  uint
  {
    name: (string-ascii 50),
    input-items: (list 5 uint),
    output-item-name: (string-ascii 50),
    output-tier: uint,
    creator: principal,
    validated: bool,
    validation-votes: uint,
    royalty-rate: uint ;; basis points (e.g., 250 = 2.5%)
  }
)

(define-map recipe-validators
  {recipe-id: uint, validator: principal}
  bool
)

(define-map user-inventories
  {user: principal, item-id: uint}
  uint ;; quantity
)

(define-map base-materials
  principal
  {
    wood: uint,
    stone: uint,
    fiber: uint,
    crystal: uint
  }
)

(define-map user-stats
  principal
  {
    items-crafted: uint,
    recipes-discovered: uint,
    total-royalties: uint
  }
)

;; Public Functions

;; Initialize player with base materials
(define-public (initialize-player)
  (let ((caller tx-sender))
    (ok (map-set base-materials caller
      {
        wood: u100,
        stone: u100,
        fiber: u100,
        crystal: u50
      }
    ))
  )
)

;; Submit a new recipe for validation
(define-public (submit-recipe 
  (recipe-name (string-ascii 50))
  (input-items (list 5 uint))
  (output-name (string-ascii 50))
  (output-tier uint)
  (royalty-rate uint))
  (let
    (
      (new-recipe-id (+ (var-get recipe-nonce) u1))
    )
    (asserts! (<= royalty-rate u1000) err-invalid-recipe) ;; Max 10% royalty
    (asserts! (and (>= output-tier u1) (<= output-tier u3)) err-invalid-recipe)
    
    (map-set recipes new-recipe-id
      {
        name: recipe-name,
        input-items: input-items,
        output-item-name: output-name,
        output-tier: output-tier,
        creator: tx-sender,
        validated: false,
        validation-votes: u0,
        royalty-rate: royalty-rate
      }
    )
    
    (var-set recipe-nonce new-recipe-id)
    
    ;; Mint governance tokens for recipe submission
    (try! (ft-mint? governance-token u10 tx-sender))
    
    (ok new-recipe-id)
  )
)

;; Validate a recipe (community consensus)
(define-public (validate-recipe (recipe-id uint))
  (let
    (
      (recipe (unwrap! (map-get? recipes recipe-id) err-not-found))
      (already-validated (default-to false (map-get? recipe-validators {recipe-id: recipe-id, validator: tx-sender})))
    )
    (asserts! (not already-validated) err-already-exists)
    (asserts! (not (get validated recipe)) err-already-exists)
    
    (map-set recipe-validators {recipe-id: recipe-id, validator: tx-sender} true)
    
    (let
      (
        (new-vote-count (+ (get validation-votes recipe) u1))
        (is-now-validated (>= new-vote-count (var-get validation-threshold)))
      )
      (map-set recipes recipe-id (merge recipe {
        validation-votes: new-vote-count,
        validated: is-now-validated
      }))
      
      ;; Reward validator with governance tokens
      (try! (ft-mint? governance-token u5 tx-sender))
      
      (ok is-now-validated)
    )
  )
)

;; Craft an item using a validated recipe
(define-public (craft-item (recipe-id uint))
  (let
    (
      (recipe (unwrap! (map-get? recipes recipe-id) err-not-found))
      (new-item-id (+ (var-get item-nonce) u1))
    )
    (asserts! (get validated recipe) err-invalid-recipe)
    
    ;; Create the NFT item
    (try! (nft-mint? crafted-item new-item-id tx-sender))
    
    (map-set items new-item-id
      {
        name: (get output-item-name recipe),
        tier: (get output-tier recipe),
        creator: tx-sender,
        recipe-id: recipe-id
      }
    )
    
    ;; Add to inventory
    (map-set user-inventories {user: tx-sender, item-id: new-item-id} u1)
    
    (var-set item-nonce new-item-id)
    
    ;; Update user stats
    (update-user-stats-crafted tx-sender)
    
    ;; Pay royalty to recipe creator if applicable
    (if (> (get royalty-rate recipe) u0)
      (try! (ft-mint? governance-token (get royalty-rate recipe) (get creator recipe)))
      true
    )
    
    (ok new-item-id)
  )
)

;; Transfer crafted item NFT
(define-public (transfer-item (item-id uint) (recipient principal))
  (let
    (
      (item (unwrap! (map-get? items item-id) err-not-found))
    )
    (try! (nft-transfer? crafted-item item-id tx-sender recipient))
    
    ;; Update inventories
    (map-delete user-inventories {user: tx-sender, item-id: item-id})
    (map-set user-inventories {user: recipient, item-id: item-id} u1)
    
    (ok true)
  )
)

;; Harvest base materials (regenerating resources)
(define-public (harvest-materials)
  (let
    (
      (current-materials (default-to {wood: u0, stone: u0, fiber: u0, crystal: u0} 
        (map-get? base-materials tx-sender)))
    )
    (ok (map-set base-materials tx-sender
      {
        wood: (+ (get wood current-materials) u10),
        stone: (+ (get stone current-materials) u10),
        fiber: (+ (get fiber current-materials) u8),
        crystal: (+ (get crystal current-materials) u2)
      }
    ))
  )
)

;; Read-only functions

(define-read-only (get-recipe (recipe-id uint))
  (ok (map-get? recipes recipe-id))
)

(define-read-only (get-item (item-id uint))
  (ok (map-get? items item-id))
)

(define-read-only (get-base-materials (user principal))
  (ok (map-get? base-materials user))
)

(define-read-only (get-inventory-item (user principal) (item-id uint))
  (ok (map-get? user-inventories {user: user, item-id: item-id}))
)

(define-read-only (get-user-stats (user principal))
  (ok (map-get? user-stats user))
)

(define-read-only (get-governance-balance (user principal))
  (ok (ft-get-balance governance-token user))
)

(define-read-only (get-item-owner (item-id uint))
  (ok (nft-get-owner? crafted-item item-id))
)

;; Private helper functions

(define-private (update-user-stats-crafted (user principal))
  (let
    (
      (current-stats (default-to {items-crafted: u0, recipes-discovered: u0, total-royalties: u0}
        (map-get? user-stats user)))
    )
    (map-set user-stats user (merge current-stats {
      items-crafted: (+ (get items-crafted current-stats) u1)
    }))
  )
)

;; Initialize contract
(begin
  (try! (ft-mint? governance-token u1000000 contract-owner))
  (ok true)
)