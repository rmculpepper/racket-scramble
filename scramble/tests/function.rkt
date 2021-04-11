#lang racket/base
(require rackunit
         scramble/function)

(define always-zero (K 0))
(check-equal? (map always-zero '(1 2 3))
              '(0 0 0))

(check-equal? (let-values ([(x y) ((K #f #f))]) 'ok)
              'ok)

(check-equal? (procedure-arity (K 'hello))
              (arity-at-least 0))
(check-equal? (procedure-arity (K0 'hello))
              0)

(check-equal? (map call
                   (list (lambda () 1)
                         (lambda () 2)))
              '(1 2))
(check-equal? (map call
                   (list add1 sub1)
                   (list 1 2))
              '(2 1))
