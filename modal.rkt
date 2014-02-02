#lang racket

(require redex
         redex/gui
         rackunit)

(define-language λ□
  ;; Types
  (τ ::= b
         (τ → τ)
         (□ τ))
  ;; Terms
  (e ::= x
         u
         ;; Functions
         (λ (x : τ) e)
         (e e)
         (fix e)
         ;; □es
         (box e)
         (let [box u e]
           e)
         ;; numbers
         natural
         (+ e e)
         (if0 e e e))
  ;; Value variable contexts
  (Γ ::= · (Γ & x  : τ))
  ;; Expression variable contexts
  (Δ ::= · (Δ & u :: τ))

  (b ::= Nat A B)
  (x ::= variable-not-otherwise-mentioned)
  (u ::= variable-not-otherwise-mentioned))

(define-metafunction λ□
  lookupVal  : Γ x -> τ or #f
  [(lookupVal · x)
   #f]
  [(lookupVal (Γ & x_1 : τ) x_1)
   τ]
  [(lookupVal (Γ & x_1 : τ) x_2)
   (lookupVal Γ x_2)])

(define-metafunction λ□
  lookupExp  : Δ u -> τ or #f
  [(lookupExp · u)
   #f]
  [(lookupExp (Δ & u_1 :: τ) u_1)
   τ]
  [(lookupExp (Δ & u_1 :: τ) u_2)
   (lookupExp Δ u_2)])

(define-judgment-form λ□
  #:mode     (tc I I I O)
  #:contract (tc Δ Γ e τ)
  [----------------- "TNat"
   (tc Δ Γ number Nat)]
  [(where τ (lookupVal Γ x))
   ---------------------- "Tvvar"
   (tc Δ Γ x τ)]
  [(where τ (lookupExp Δ x))
   ---------------------- "Tevar"
   (tc Δ Γ x τ)]
  [(tc Δ (Γ & x : τ_1) e τ_2)
   ---------------------- "Tλ"
   (tc Δ Γ (λ (x : τ_1) e) (τ_1 → τ_2))]
  [(tc Δ Γ e (τ → τ))
   ---------------------- "Tfix"
   (tc Δ Γ (fix e) τ)]
  [(tc Δ Γ e_1 (τ_1 → τ_2))
   (tc Δ Γ e_2 τ_1)
   ---------------------  "Tapp"
   (tc Δ Γ (e_1 e_2) τ_2)]
  [(tc Δ · e τ)
   ---------------------- "TBox"
   (tc Δ Γ (box e) (□ τ))]
  [(tc Δ Γ e_1 (□ τ_1))
   (tc (Δ & u :: τ_1) Γ e_2 τ_2)
   ----------------------- "TunBox"
   (tc Δ Γ (let [box u e_1]
             e_2)            τ_2)]
  [(tc Δ Γ e_1 Nat)
   (tc Δ Γ e_2 Nat)
   ---------------------- "Tplus"
   (tc Δ Γ (+ e_1 e_2) Nat)]
  [(tc Δ Γ e_1 Nat)
   (tc Δ Γ e_2 τ)
   (tc Δ Γ e_3 τ)
   ---------------------- "Tif0"
   (tc Δ Γ (if0 e_1 e_2 e_3) τ)])

;; Some small Tests/examples
(check-true
 (judgment-holds
  (tc ·
      ·
      (λ (bx : (□ A))
         (let [box x bx]
           (box x)))
      ((□ A) → (□ A)))))

(check-true
 (judgment-holds
  (tc ·
      ·
      (λ (bx : (□ A))
         (let [box x bx]
           x))
      ((□ A) → A))))

(check-true
 (judgment-holds
  (tc ·
      ·
      (λ (bx : (□ A))
         (let [box x bx]
           (box (box x))))
      ((□ A) → (□ (□ A))))
  ))

(check-true
 (judgment-holds
  (tc ·
      ·
      (λ (bp : (□ (A → B)))
         (λ (bq : (□ A))
            (let [box p bp]
              (let [box q bq]
                (box (p q))))))
      ((□ (A → B)) → ((□ A) → (□ B))))))
