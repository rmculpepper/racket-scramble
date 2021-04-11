;; Copyright 2020-2021 Ryan Culpepper
;; Licensed under the Apache License, Version 2.0

#lang racket/base
(provide singleton?)

(define (singleton? v)
  (and (pair? v) (null? (cdr v))))
