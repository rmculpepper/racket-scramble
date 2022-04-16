;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require rackunit
         scramble/number)

(check-equal? (min* 1 -inf.0)
              -inf.0)
(check-equal? (min* 1 +inf.0)
              1)
(check-equal? (min* 1 4.0)
              1.0)
(check-equal? (min* +inf.0 +nan.0)
              +nan.0)

(check-equal? (max* 10 -inf.0)
              10)
(check-equal? (max* 10 +inf.0)
              +inf.0)
(check-equal? (max* +inf.0 +nan.0)
              +nan.0)

(for ([d (in-range 1 11)])
  (for ([n (in-range -40 50)])
    (check-equal? (ceiling-quotient n d)
                  (ceiling (/ n d)))
    (check-equal? (ceiling-multiple n d)
                  (* d (ceiling (/ n d))))
    (check-equal? (floor-quotient n d)
                  (floor (/ n d)))
    (check-equal? (floor-multiple n d)
                  (* d (floor (/ n d))))))
