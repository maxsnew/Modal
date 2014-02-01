#lang racket

(require redex)

(define-language λ□
  ;; Types
  (A ::= b
         (A → A)
         (□ A))
  ;; Terms
  (e ::= x
         u
         (λ (x : A) e)
         (e e)
         (box e)
         (let [box u e]
           e))
  ;; Value variable contexts
  (Γ ::= · (Γ & x  : A))
  ;; Expression variable contexts
  (Δ ::= · (Δ & u :: A))

  (b ::= Nat)
  (x ::= variable-not-otherwise-mentioned)
  (u ::= variable-not-otherwise-mentioned))

(define-judgment-form λ□
  #:mode     (tc I I I O)
  #:contract (tc Δ Γ e A)
  [(where A (lookup Γ x))
   ---------------------- "Tvvar"
   (tc Δ Γ x A)]
  [(where A (lookup Δ x))
   ---------------------- "Tevar"
   (tc Δ Γ x A)]
  [(tc Δ (Γ & x : A) e A)
   ---------------------- "Tλ"
   (tc Δ Γ (λ (x A) e) (A → B))]
  [(tc Δ Γ e_1 (A → B))
   (tc Δ Γ e_2 A)
   ---------------------  "Tapp"
   (tc Δ Γ (e_1 e_2) B)]
  [(tc Δ · e A)
   ---------------------- "TBox"
   (tc Δ Γ (box e) (□ A))]
  [(tc Δ Γ e_1 (□ A))
   (tc (Δ & u :: A) Γ e_2 B)
   ----------------------- "TunBox"
   (tc Δ Γ (let [box u e_1]
             e_2)            B)])
