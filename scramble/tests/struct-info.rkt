#lang racket/base

(module pt-base racket/base
  (struct point (x y) #:prefab)
  (provide (struct-out point)))

(module pt-mod racket/base
  (require (for-syntax racket/base scramble/struct-info)
           racket/match
           (rename-in (submod ".." pt-base) [point real-point]))

  (define (make-point x [y x])
    (real-point x y))

  (define-syntax point
    (adjust-struct-info (syntax-local-value #'real-point)
                        #:constructor #'make-point
                        #:match-expander
                        (lambda (stx)
                          (syntax-case stx ()
                            [(_ x-pat y-pat) #'(real-point x-pat y-pat)]
                            [(_ x-pat) #'(real-point x-pat 0)]))))
  (provide (struct-out point)))

(module test racket/base
  (require rackunit
           racket/match
           (submod ".." pt-mod))

  (struct point3 point (z) #:prefab)
  (check-equal? (procedure-arity point) '(1 2))
  (check-equal? (point 5) (point 5 5))
  (check-pred point? (point3 1 2 3))

  (check-equal? (match (point 1 2)
                  [(point x y) (+ x y)])
                3)

  (define (y-zero? p)
    (match p
      [(point x) #t]
      [_ #f]))
  (check-equal? (y-zero? (point 5 0)) #t)
  (check-equal? (y-zero? (point 1 2)) #f))
