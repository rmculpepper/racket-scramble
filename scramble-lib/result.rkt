;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         racket/contract/base
         racket/contract/combinator)
(provide (struct-out ok)
         (struct-out bad)

         result?
         result/c

         partition-results)

;; Result is strictly tagged:
;; (Result X Y) = (ok X) | (bad Y)

(struct ok (value) #:prefab)
(struct bad (value) #:prefab)

(define (result? v) (or (ok? v) (bad? v)))

;; ----------------------------------------

(define (result/c okc [badc any/c])
  (-result/c 'result/c okc badc))

(define (-result/c who okc badc)
  (let ([okc (coerce-contract who okc)]
        [badc (coerce-contract who badc)])
    (cond [(and (flat-contract? okc) (flat-contract? badc))
           (ctc:flat okc badc)]
          [(and (chaperone-contract? okc) (chaperone-contract? badc))
           (ctc:chaperone okc badc)]
          [else (ctc:impersonator okc badc)])))

;; ----------------------------------------

(define (ctc-name ctc)
  (match-define (ctc:base okc badc) ctc)
  `(result/c ,(contract-name okc) ,(contract-name badc)))

(define (ctc-first-order ctc)
  (match-define (ctc:base okc badc) ctc)
  (define ok-fo (contract-first-order okc))
  (define bad-fo (contract-first-order badc))
  (lambda (v)
    (cond [(ok? v) (ok-fo (ok-value v))]
          [(bad? v) (bad-fo (bad-value v))]
          [else #f])))

(define (ctc-late-neg-projection ctc)
  (match-define (ctc:base okc badc) ctc)
  (define ok-lnp (get/build-late-neg-projection okc))
  (define bad-lnp (get/build-late-neg-projection badc))
  (lambda (blame)
    (define ok-proj (ok-lnp (blame-add-context blame "the ok value of")))
    (define bad-proj (bad-lnp (blame-add-context blame "the bad value of")))
    (lambda (v neg-party)
      (cond [(ok? v)
             (define v1 (ok-value v))
             (define v2 (ok-proj v1))
             (if (eq? v1 v2) v (ok v2))]
            [(bad? v)
             (define v1 (bad-value v))
             (define v2 (bad-proj v1))
             (if (eq? v1 v2) v (bad v2))]
            [else
             (raise-blame-error blame v #:missing-party neg-party
                                '(expected: "result?" given: "~v") v)]))))

(struct ctc:base (okc badc) #:transparent)

(struct ctc:flat ctc:base () #:transparent
  #:property prop:flat-contract
  (build-flat-contract-property
   #:first-order ctc-first-order
   #:name ctc-name))
(struct ctc:chaperone ctc:base () #:transparent
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:late-neg-projection ctc-late-neg-projection
   #:first-order ctc-first-order
   #:name ctc-name))
(struct ctc:impersonator ctc:base () #:transparent
  #:property prop:contract
  (build-contract-property
   #:late-neg-projection ctc-late-neg-projection
   #:first-order ctc-first-order
   #:name ctc-name))

;; ============================================================
;; Result

;; ok : X -> (Result X Y)
;; ok? : (Result X Y) -> Boolean
;; ok-value : (Result X Y) -> X

#|
(define-syntax (result-do stx)
  (syntax-case stx ()
    [(_ ([x rhs] ...) body0 body ...)
     (foldr (lambda (x rhs body)
              (quasisyntax/loc stx
                (let ([r #,rhs])
                  (cond [(ok? r) (let ([#,x (ok-value r)]) #,body)]
                        [(bad? r) r]
                        [else (raise-argument-error 'result-do "result?" r)]))))
            (syntax/loc stx (let () body0 body ...))
            (syntax->list #'(x ...))
            (syntax->list #'(rhs ...)))]))

;; result-bind : (Result X Z) (X -> (Result Y Z)) [#:bad-f (or/c #f (Z -> (Result Y Z)))]
;;            -> (Result Y Z)
(define (result-bind c f #:bad-f [bad-f #f])
  (cond [(ok? c) (f (ok-value c))]
        [(bad? c) (if bad-f (bad-f (bad-value c)) c)]
        [else (raise-argument-error 'result-bind "result?" c)]))

;; result-or : (Result X Y) ... -> (Result X Y)
;; Returns leftmost ok result, or else rightmost bad result.
(define-syntax (result-or stx)
  (syntax-case stx ()
    [(_ ce ... ce*)
     (foldr (lambda (ce1 ce2)
              (quasisyntax/loc stx
                (let ([r #,ce1])
                  (cond [(ok? r) r]
                        [(bad? r) #,ce2]
                        [else (raise-argument-error 'result-or "result?" r)]))))
            #'r ;; list is non-empty, so bound by last generated let
            (syntax->list #'(ce ... ce*)))]))

;; result-ormap : (X -> (Result Y Z)) (NEListof X) -> (Result Y Z)
(define (result-ormap f vs)
  (match vs
    [(list v) (f v)]
    [(cons v vs) (result-or (f v) (result-ormap f vs))]))

;; result-and : (Result X Y) ... -> (Result X Y)
;; Returns leftmost bad result, or else rightmost ok result.
(define-syntax (result-and stx)
  (syntax-case stx ()
    [(_ ce ... ce*)
     (foldr (lambda (ce1 ce2)
              (quasisyntax/loc stx
                (let ([r #,ce1])
                  (cond [(ok? r) #,ce2]
                        [(bad? r) r]
                        [else (raise-argument-error 'result-and "result?" r)]))))
            #'r ;; list is non-empty, so bound by last generated let
            (syntax->list #'(ce ... ce*)))]))

;; result-andmap : (X -> (Result Y Z)) (NEListof X) -> (Result Y Z)
(define (result-andmap f vs)
  (match vs
    [(list v) (f v)]
    [(cons v vs) (result-and (f v) (result-andmap f vs))]))
|#

;; partition-results : (Listof (Result  X Y)) #t -> (Listof X) (Listof Y)
(define (partition-results rs)
  (define (wrong) (raise-argument-error 'partition-results "(listof result?)" rs))
  (let loop ([rs rs] [rgoodvs null] [rbadvs null])
    (match rs
      ['() (values (reverse rgoodvs) (reverse rbadvs))]
      [(cons (ok goodv) rs)
       (loop rs (cons goodv rgoodvs) rbadvs)]
      [(cons (bad badv) rs)
       (loop rs rgoodvs (cons badv rbadvs))]
      [_ (wrong)])))
