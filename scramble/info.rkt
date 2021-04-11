#lang info

;; ========================================
;; pkg info

(define collection "scramble")
(define deps
  '("base"
    "scramble-lib"
    "rackunit-lib"))
(define implies
  '("scramble-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"))

;; ========================================
;; collect info

(define name "scramble")
(define scribblings
  '(["scribblings/scramble.scrbl" (#;multi-page)]))
