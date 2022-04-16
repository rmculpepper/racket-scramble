;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require (for-syntax racket/base)
         rackunit
         scramble/inject-syntax
         racket/list)

;; ----

(define-for-syntax do-require? #t)

(begin/inject-syntax
  (if do-require? #'(require racket/string) #'(begin)))

(define (my-string-join xs sep)
  (error "using my string-join!"))

(begin/inject-syntax
  (if (identifier-binding #'string-join)
      #'(begin)
      #'(define string-join my-string-join)))

(check-equal? (string-join '("hello" "world!") " ")
              "hello world!")

;; ----

(define (my-string-exclaim str)
  (regexp-replace* #rx"[.]" str "!"))

(begin/inject-syntax
  (if (identifier-binding #'string-exclaim)
      #'(begin)
      #'(define string-exclaim my-string-exclaim)))

(check-equal? (string-exclaim "Hello. Nice to see you.")
              "Hello! Nice to see you!")

;; ----

(module slow-popcount racket/base
  (provide slow-popcount)
  (define (slow-popcount n)
    (cond [(zero? n) 0]
          [else (+ (slow-popcount (quotient n 2))
                   (if (bitwise-bit-set? n 0) 1 0))])))
(require (for-syntax 'slow-popcount) 'slow-popcount)

(define (faster-popcount n)
  (define (byte-popcount b)
    (bytes-ref (begin/inject-syntax
                 #`(quote #,(apply bytes (for/list ([n 256]) (slow-popcount n)))))
               b))
  (cond [(zero? n) 0]
        [else (+ (faster-popcount (arithmetic-shift n -8))
                 (byte-popcount (bitwise-bit-field n 0 8)))]))

(for ([i 100])
  (define n (random #e1e6))
  (check-equal? (faster-popcount n)
                (slow-popcount n)))
