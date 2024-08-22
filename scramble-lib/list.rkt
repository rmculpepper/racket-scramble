;; Copyright 2020-2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(provide singleton?
         append-reverse)

(define (singleton? v)
  (and (pair? v) (null? (cdr v))))

(define (append-reverse xs onto)
  ;; (append (reverse xs) onto)
  (unless (list? xs) (raise-argument-error 'append-reverse "list?" 0 xs onto))
  (for/fold ([onto onto]) ([x (in-list xs)]) (cons x onto)))
