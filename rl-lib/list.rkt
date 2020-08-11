#lang racket/base
(provide singleton?)

(define (singleton? v)
  (and (pair? v) (null? (cdr v))))
