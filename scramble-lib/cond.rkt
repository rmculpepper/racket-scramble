;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require (for-syntax racket/base syntax/parse)
         syntax/srcloc)
(provide cond+ and+ or+)

(begin-for-syntax
  (define-syntax-class expr*
    #:description "expression (not the literal `else`)"
    #:literals (else)
    (pattern (~and _:expr (~not else))))
  (define-splicing-syntax-class do-clause
    #:attributes (gen) ;; Syntax[Expr] -> Syntax[Expr]
    #:description "#:do clause"
    (pattern (~seq #:do form:expr) ;; Note: multiple #:do definitions have let*-like scoping
             #:attr gen (lambda (tail)
                          #`(let () form #,tail))))
  (define-splicing-syntax-class clause
    #:attributes (gen) ;; Syntax[Expr] -> Syntax[Expr]
    #:description "cond+ clause"
    #:literals (=>)
    (pattern [test:expr* => ~! fun:expr]
             #:attr gen (lambda (tail)
                          #`(let ([t test]) (if t (fun t) #,tail))))
    (pattern [test:expr* body:expr ...+]
             #:attr gen (lambda (tail)
                          #`(if test (let () body ...) #,tail)))
    (pattern [test:expr*]
             #:attr gen (lambda (tail)
                          #`(let ([t test]) (if t t #,tail))))
    (pattern :do-clause))
  (define-splicing-syntax-class final-clause #:attributes (code)
    #:description "final cond+ clause"
    #:literals (else)
    (pattern (~seq #:else ~! body:expr ...+)
             #:with code #'(let () body ...))
    (pattern [else ~! body:expr ...+]
             #:with code #'(let () body ...))))

(define-syntax cond+
  (syntax-parser
    [(_ c:clause ... (~optional f:final-clause))
     (foldr (lambda (f a) (f a))
            (or (attribute f.code)
                #`(error 'cond+ "all clauses failed\n  location: ~a"
                         (source-location->string
                          (quote-syntax #,(datum->syntax #f 'HERE this-syntax)))))
            (attribute c.gen))]))

(define-syntax (and+ stx)
  (define-splicing-syntax-class part
    #:attributes (gen) ;; Syntax[Expr] -> Syntax[Expr]
    #:description #f
    (pattern e:expr #:attr gen (lambda (tail) #`(and e #,tail)))
    (pattern :do-clause))
  (syntax-parse stx
    [(_ e:part ... e*:expr)
     (foldr (lambda (f a) (f a))
            #'e*
            (attribute e.gen))]))

(define-syntax (or+ stx)
  (define-splicing-syntax-class part
    #:attributes (gen) ;; Syntax[Expr] -> Syntax[Expr]
    #:description #f
    (pattern e:expr #:attr gen (lambda (tail) #`(or e #,tail)))
    (pattern :do-clause))
  (syntax-parse stx
    [(_ e:part ... e*:expr)
     (foldr (lambda (f a) (f a))
            #'e*
            (attribute e.gen))]))
