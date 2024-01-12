#lang racket/base
(provide let-return)

(define-syntax let-return
  (syntax-parser
    [(_ ([var:id rhs:expr] ...) body:expr ...)
     #'(let ([var rhs] ...)
         (let ()
           body ...
           (void))
         (values var ...))]))
