;; Copyright 2020-2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/contract/base)
(provide (contract-out
          #:unprotected-submodule unchecked
          [exact
           (-> number? any)]
          [inexact
           (-> number? any)]
          [min*
           (->* [] [] #:rest (listof real?) real?)]
          [max*
           (->* [] [] #:rest (listof real?) real?)]
          [ceiling-quotient
           (-> exact-integer? exact-positive-integer? exact-integer?)]
          [ceiling-multiple
           (-> exact-integer? exact-positive-integer? exact-integer?)]
          [floor-quotient
           (-> exact-integer? exact-positive-integer? exact-integer?)]
          [floor-multiple
           (-> exact-integer? exact-positive-integer? exact-integer?)]
          ))

(define exact inexact->exact)
(define inexact exact->inexact)

;; Like min/max, but comparisons with infinities don't produce inexact.
(define (min2 a b)
  (define (nan? x) (= x +nan.0))
  (cond [(or (nan? a) (nan? b)) +nan.0]
        [(= a +inf.0) b]
        [(= b +inf.0) a]
        [else (min a b)]))
(define (max2 a b)
  (define (nan? x) (= x +nan.0))
  (cond [(or (nan? a) (nan? b)) +nan.0]
        [(= a -inf.0) b]
        [(= b -inf.0) a]
        [else (max a b)]))

(define min*
  (case-lambda
    [() +inf.0]
    [(x y) (min2 x y)]
    [(x . xs) (foldl min2 x xs)]))
(define max*
  (case-lambda
    [() -inf.0]
    [(x y) (max2 x y)]
    [(x . xs) (foldl max2 x xs)]))

;; ----------------------------------------
;; Division and multiples
;; where n is exact integer, d is positive integer

;; Note: quotient is quotient-truncate, not quotient-floor.

;; FIXME: maybe use gmp naming convention: cdiv, fdiv, tdiv ??
;; FIXME: maybe use SRFI-141 names?

(define (ceiling-quotient n d)
  #;(ceiling (/ n d))
  (if (< n 0)
      (quotient n d)
      (quotient (+ n d -1) d)))

(define (ceiling-multiple n d)
  #;(* (ceiling-quotient n d) d)
  (+ n (modulo (- n) d)))

(define (floor-quotient n d)
  (- (ceiling-quotient (- n) d)))

(define (floor-multiple n d)
  #;(* (quotient n d) d)
  (- n (modulo n d)))
