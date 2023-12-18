;; Copyright 2020-2023 Ryan Culpepper
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
          [string->integer
           (->* [string?]
                [#:radix (integer-in 2 16) #:min real? #:max real?]
                (or/c exact-integer? #f))]
          )

         MIN-INT16
         MAX-INT16
         MAX-UINT16

         MIN-INT32
         MAX-INT32
         MAX-UINT32

         MIN-INT64
         MAX-INT64
         MAX-UINT64

         int16?
         int32?
         int64?

         uint16?
         uint32?
         uint64?)

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

;; ----------------------------------------
;; String conversion

(define (string->integer s
                         #:radix [radix 10]
                         #:min [minimum -inf.0]
                         #:max [maximum +inf.0])
  (define n (string->number s radix))
  (and n (<= minimum n maximum) n))

;; ----------------------------------------
;; Limits

(define MIN-INT16 (- (expt 2 15)))
(define MAX-INT16 (sub1 (expt 2 15)))
(define MAX-UINT16 (sub1 (expt 2 16)))

(define MIN-INT32 (- (expt 2 31)))
(define MAX-INT32 (sub1 (expt 2 31)))
(define MAX-UINT32 (sub1 (expt 2 32)))

(define MIN-INT64 (- (expt 2 63)))
(define MAX-INT64 (sub1 (expt 2 63)))
(define MAX-INT64 (sub1 (expt 2 63)))

(define (int16? v) (and (exact-integer? v) (<= MIN-INT16 v MAX-INT16)))
(define (int32? v) (and (exact-integer? v) (<= MIN-INT32 v MAX-INT32)))
(define (int64? v) (and (exact-integer? v) (<= MIN-INT64 v MAX-INT64)))

(define (uint16? v) (and (exact-integer? v) (<= 0 v MAX-UINT16)))
(define (uint32? v) (and (exact-integer? v) (<= 0 v MAX-UINT32)))
(define (uint64? v) (and (exact-integer? v) (<= 0 v MAX-UINT64)))
