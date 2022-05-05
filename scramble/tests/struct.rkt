#lang racket/base
(require rackunit
         scramble/struct)

(struct point (x y)
  #:property prop:auto-equal+hash #t)

(check-equal? (point 1 2) (point 1 2))
(check-not-equal? (point 1 2) (point 0 0))

(check-equal? (equal-hash-code (point 1 2))
              (equal-hash-code (point 1 2)))
(check-not-equal? (equal-hash-code (point 1 2))
                  (equal-hash-code (point 0 0)))

(struct point3 point (z color)
  #:property prop:auto-equal+hash (list (struct-field-index z)))

(check-equal? (point3 1 2 3 #f) (point3 1 2 3 'red))
(check-not-equal? (point3 0 0 3 'red) (point3 1 2 3 'red))

(check-equal? (equal-hash-code (point3 1 2 3 #f))
              (equal-hash-code (point3 1 2 3 'red)))
