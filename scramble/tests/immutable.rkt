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


(define (apply-hash-c h) (contract (hash/c symbol? real?) h 'pos 'neg))

(let ([test-values
       (list (string-copy "abc")
             "abc"

             (bytes-copy #"abc")
             #"abc"

             (vector 1 2 3)
             (vector-immutable 1 2 3)
             (contract (vectorof real?) (vector 1 2 3) 'pos 'neg)
             (contract (vectorof real?) (vector-immutable 1 2 3) 'pos 'neg)

             (box 1)
             (box-immutable 1)
             (contract (box/c real?) (box 1) 'pos 'neg)
             (contract (box/c real?) (box-immutable 1) 'pos 'neg)

             (make-hash '((a . 1) (b . 2)))
             (hash 'a 1 'b 2)
             (apply-hash-c (make-hash '((a . 1) (b . 2))))
             (apply-hash-c (hash 'a 1 'b 2))

             (make-hasheq '((a . 1) (b . 2)))
             (hasheq 'a 1 'b 2)
             (apply-hash-c (make-hasheq '((a . 1) (b . 2))))
             (apply-hash-c (hasheq 'a 1 'b 2))

             (make-hasheqv '((a . 1) (b . 2)))
             (hasheqv 'a 1 'b 2)
             (apply-hash-c (make-hasheqv '((a . 1) (b . 2))))
             (apply-hash-c (hasheqv 'a 1 'b 2))

             (make-hashalw '((a . 1) (b . 2)))
             (hashalw 'a 1 'b 2)
             (apply-hash-c (make-hashalw '((a . 1) (b . 2))))
             (apply-hash-c (hashalw 'a 1 'b 2))

             (make-weak-hash '((a . 1) (b . 2)))
             (make-weak-hasheq '((a . 1) (b . 2)))
             (make-weak-hasheqv '((a . 1) (b . 2)))
             (make-weak-hashalw '((a . 1) (b . 2)))

             (apply-hash-c (make-weak-hash '((a . 1) (b . 2))))
             (apply-hash-c (make-weak-hasheq '((a . 1) (b . 2))))
             (apply-hash-c (make-weak-hasheqv '((a . 1) (b . 2))))
             (apply-hash-c (make-weak-hashalw '((a . 1) (b . 2))))

             (make-ephemeron-hash '((a . 1) (b . 2)))
             (make-ephemeron-hasheq '((a . 1) (b . 2)))
             (make-ephemeron-hasheqv '((a . 1) (b . 2)))
             (make-ephemeron-hashalw '((a . 1) (b . 2)))

             (apply-hash-c (make-ephemeron-hash '((a . 1) (b . 2))))
             (apply-hash-c (make-ephemeron-hasheq '((a . 1) (b . 2))))
             (apply-hash-c (make-ephemeron-hasheqv '((a . 1) (b . 2))))
             (apply-hash-c (make-ephemeron-hashalw '((a . 1) (b . 2))))
             )])

  (for ([v (in-list test-values)])
    (define (mutable? v) (not (immutable? v)))
    (define (authentic? v) (not (impersonator? v)))

    (define imm-v (immutable v))
    (check-pred immutable-authentic? imm-v)
    (when (immutable-authentic? v)
      (check-eq? imm-v v))

    (define mut-v (mutable v #f))
    (check-pred mutable? mut-v)
    (check-pred authentic? mut-v)

    (define fresh-mut-v (mutable v #t))
    (check-pred mutable? fresh-mut-v)
    (check-pred authentic? fresh-mut-v)
    (check-not-eq? fresh-mut-v v)))
