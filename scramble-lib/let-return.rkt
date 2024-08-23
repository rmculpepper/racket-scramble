;; Copyright 2024 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require (for-syntax racket/base))
(provide let-return)

(define-syntax (let-return stx)
  (syntax-case stx ()
    [(_ ([var rhs] ...) . body)
     (let ()
       (define vars (syntax->list #'(var ...)))
       (for ([var (in-list vars)])
         (unless (identifier? var)
           (raise-syntax-error #f "expected identifier" stx #'var)))
       (let ([dup (check-duplicate-identifier vars)])
         (when dup (raise-syntax-error #f "duplicate identifier" stx dup)))
       (syntax/loc stx
         (let ([var rhs] ...)
           (begin . body)
           (#%expression (values var ...)))))]))
