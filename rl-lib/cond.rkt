#lang racket/base
(require (for-syntax racket/base syntax/parse))
(provide cond+)

(begin-for-syntax
  (define-syntax-class expr*
    #:description "expression (not the literal `else`)"
    #:literals (else)
    (pattern (~and _:expr (~not else))))
  (define-splicing-syntax-class clause
    #:attributes (gen) ;; (Syntax -> Syntax)
    #:description "cond+ clause"
    #:literals (=>)
    (pattern [test:expr* => ~! fun:expr]
             #:attr gen (lambda (or-else)
                          #`(let ([t test]) (if t (fun t) #,or-else))))
    (pattern [test:expr* body:expr ...+]
             #:attr gen (lambda (or-else)
                          #`(if test (let () body ...) #,or-else)))
    (pattern (~seq #:do form:expr) ;; Note: multiple #:do definitions have let*-like scoping
             #:attr gen (lambda (or-else)
                          #`(let () form #,or-else))))
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
                #'(error 'cond+ "all clauses failed\n  location: ~a"
                         (quote-syntax #,(datum-syntax #f 'HERE this-syntax))))
            (attribute c.gen))]))
