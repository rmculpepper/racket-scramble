#lang racket/base
(require (submod racket/performance-hint begin-encourage-inline))
(provide K0 K1 K K* call call*)

;; ----------------------------------------
;; Creating constant functions

(begin-encourage-inline

  ;; result has arity 0
  (define K0
    (case-lambda
      [(v) (lambda () v)]
      [vs  (lambda () (apply values vs))]))

  ;; result has arity 1
  (define K1
    (case-lambda
      [(v) (lambda (x) v)]
      [vs  (lambda (x) (apply values vs))]))

  ;; result has arity * (but no keywords)
  (define K
    (case-lambda
      [(v) (lambda x v)]
      [vs  (lambda x (apply values vs))])))

;; result accepts any args, any keywords
(define (K* . vs)
  (define proc (apply K vs))
  (make-keyword-procedure 
   (lambda (kws kwargs . x) (proc))
   proc))

;; ----------------------------------------
;; (call f x ...) = (f x ...)

(begin-encourage-inline
  (define call
    (case-lambda
      [(f) (f)]
      [(f x) (f x)]
      [(f . xs) (apply f xs)])))

(define call*
  (make-keyword-procedure
   (lambda (kws kwargs f . xs) (keyword-apply f kws kwargs xs))
   call))
