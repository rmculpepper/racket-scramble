#lang racket/base
(require rackunit
         scramble/cond)

(define entries
  `([x 5]
    [y ,(lambda (key) (list key 12))]))
(define (lookup key)
  (define entry (assoc key entries))
  (cond+ [(not entry) (error "not found")]
         #:do (define v (cadr entry))
         [(procedure? v) (v key)]
         #:else v))
(check-equal? (lookup 'x) 5)
(check-equal? (lookup 'y) (list 'y 12))
(check-exn #rx"not found"
           (lambda () (lookup 'z)))

(check-exn #rx"all clauses failed.*cond[.]rkt"
           (lambda ()
             (cond+ [(odd? 12) 'odd]
                    [(even? 7) 'even-odder])))

(check-equal? (and+ (even? 42)
                    #:do (define x (quotient 42 2))
                    (even? x))
              #f)
