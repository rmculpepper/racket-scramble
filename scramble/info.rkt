#lang info

;; ========================================
;; pkg info

(define collection "scramble")
(define license 'Apache-2.0)
(define deps
  '("base"
    "scramble-lib"
    "rackunit-lib"))
(define implies
  '("scramble-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"))
(define pkg-authors '(ryanc))

;; ========================================
;; collect info

(define name "scramble")
(define scribblings
  '(["scribblings/scramble.scrbl" (multi-page)]))
