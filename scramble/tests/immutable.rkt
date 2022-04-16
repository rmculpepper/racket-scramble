;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/contract
         rackunit
         scramble/immutable)

(define (imm? v)
  (and (immutable? v)
       (not (impersonator? v))))

(define (mut? v)
  (and (not (immutable? v))
       (not (impersonator? v))))

(define (check-im v)
  (check-pred imm? (immutable v))
  (check-pred mut? (mutable v))
  (check-pred mut? (mutable v #t))
  (check-pred mut? (mutable v #f))
  (unless (hash? v)
    (check-equal? v (immutable v))
    (check-equal? v (mutable v)))
  (when (imm? v)
    (check-eq? (immutable v) v))
  (when (mut? v)
    (check-eq? (mutable v #f) v)))

(check-im (make-bytes 10 0))
(check-im #"abc")
(check-im (make-string 10 #\A))
(check-im "ABC")
(check-im (make-vector 10))
(check-im #(1 2 3))
(check-im (box #t))
(check-im '#&100)
(check-im (make-hash '((a . 1) (b . 2))))
(check-im (hasheq 'a 1 'b 2))

(define/contract vf (-> (vectorof string?) (vectorof string?))
  (lambda (x) x))

(check-pred impersonator? (vf (make-vector 10 "abc")))
(check-im (vf (make-vector 10 "abc")))
(check-im (vf #("a" "b" "c")))

(define/contract vb (-> (box/c symbol?) (box/c symbol?))
  (lambda (x) x))

(check-pred impersonator? (vb (box 'apple)))
(check-im (vb (box 'apple)))
(check-im (vb '#&banana))

(define/contract vh (-> (hash/c symbol? any/c) (hash/c any/c number?))
  (lambda (x) x))

(check-pred impersonator? (vh (make-hasheq '((a . 1) (b . 2)))))
(check-im (vh (make-hasheq '((a . 1) (b . 2)))))
(check-im (vh (hasheq 'a 1 'b 2)))
