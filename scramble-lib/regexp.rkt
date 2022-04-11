;; Copyright 2022 Ryan Culpepper
;; Licensed under the Apache License, Version 2.0

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/datum
                     syntax/transformer
                     (submod "private/regexp.rkt" ast)
                     (submod "private/regexp.rkt" codegen)
                     (submod "private/regexp.rkt" syntax)))
(provide define-RE px)

;; ------------------------------------------------------------
;; Syntax

(define-syntax (px stx)
  (syntax-parse stx
    [(_ re:RE ...)
     (define-values (ast px) (check-RE (make-re:cat (datum (re.ast ...)))))
     #`(quote #,px)]))

(define-syntax (define-RE stx)
  (syntax-parse stx
    [(_ name:id re:RE)
     (define-values (ast px) (check-RE (datum re.ast)))
     #`(define-syntax name (RE-binding (quote #,ast) (quote #,px)))]))
