;; Copyright 2020 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require (for-syntax racket/base))
(provide begin/inject-syntax)

(define-syntax (begin/inject-syntax stx)
  (syntax-case stx ()
    [(_ . body)
     (let ()
       (define (bad msg) (raise-syntax-error #f msg stx #'e))
       ;; The following code is almost the same as syntax-local-eval, but we
       ;; must remove the intdef scope from the resulting syntax, so that
       ;; computed definitions and requires work, for example.
       (define name (car (generate-temporaries '(begin/inject-syntax))))
       (define intdef (syntax-local-make-definition-context))
       (syntax-local-bind-syntaxes (list name) #'(lambda () . body) intdef)
       (define results
         (let ([name* (internal-definition-context-introduce intdef name)])
           (call-with-values (syntax-local-value name* #f intdef) list)))
       (cond [(and (pair? results) (null? (cdr results)))
              (define result (car results))
              (unless (syntax? result)
                (bad (format "result was not syntax\n  result: ~e" result)))
              (syntax-local-introduce
               (internal-definition-context-introduce intdef result 'remove))]
             [else
              (bad (format "result arity mismatch~a\n  expected: 1\n  received: ~s"
                           ";\n expected number of values not received"
                           (length results)))]))]))
