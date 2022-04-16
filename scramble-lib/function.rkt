;; Copyright 2020-2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require (submod racket/performance-hint begin-encourage-inline))
(provide K0 K call)

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

;; ----------------------------------------
;; (call f x ...) = (f x ...)

(begin-encourage-inline
  (define call
    (case-lambda
      [(f) (f)]
      [(f x) (f x)]
      [(f . xs) (apply f xs)])))
