;; Copyright 2019-2024 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require (for-template racket/base))
(provide datum->expression)

;; A DatumPlus is one of
;; - atomic quotable -- number, boolean, string, regexp, etc
;; - (Pair DatumPlus DatumPlus)
;; - (Vectorof DatumPlus)
;; - (make-prefab-struct PrefabKey DatumPlus ...)
;; - (Hashof DatumPlus DatumPlus) -- hash-{equal?, equal-always?, eqv?, eq?}

(define (atomic? v)
  (or (null? v) (boolean? v) (number? v) (char? v)
      (string? v) (bytes? v) (symbol? v) (keyword? v)
      (regexp? v) (byte-regexp? v)))

(define (immutable-struct-type? stype)
  (define-values (name init-field-n _afn _get _set imm-fields super-type _skip)
    (struct-type-info stype))
  (and (= init-field-n (length imm-fields))
       (if super-type (immutable-struct-type? stype) #t)))

;; datum->expression : DatumPlus -> Syntax[Expr]
(define (datum->expression v0 #:convert [convert (lambda (v) #f)])
  (define syntax-h (make-hash))
  (define (make-syntax-ref stx)
    (hash-ref! syntax-h stx (lambda () (car (generate-temporaries '(stx))))))
  (define (const v) `(quote ,v))
  (define (const? e) (and (pair? e) (eq? (car e) 'quote)))
  (define (bad v)
    (error 'datum->expression "expected quotable value\n  got: ~e\n  within: ~e"
           v v0))
  (define (loop v)
    (cond [(atomic? v)
           (const v)]
          [(syntax? v)
           (make-syntax-ref `(quote-syntax ,v))]
          [(pair? v)
           (cond [(and (list? v) (andmap identifier? v))
                  `(syntax->list (quote-syntax ,(datum->syntax #f v)))]
                 [else
                  (define outer-v v)
                  (let pairloop ([v v] [acc null])
                    (cond [(pair? v)
                           (pairloop (cdr v) (cons (loop (car v)) acc))]
                          [(null? v)
                           (cond [(andmap const? acc) (const outer-v)]
                                 [else `(list ,@(reverse acc))])]
                          [else
                           (let ([acc (cons (loop v) acc)])
                             (cond [(andmap const? acc) (const outer-v)]
                                   [else `(list* ,@(reverse acc))]))]))])]
          [(vector? v)
           (let ([elem-es (map loop (vector->list v))])
             (cond [(andmap const? elem-es) (const v)]
                   [else `(vector-immutable ,@elem-es)]))]
          [(prefab-struct-key v)
           => (lambda (key)
                (define elem-es (map loop (cdr (vector->list (struct->vector v)))))
                (cond [(and (andmap const? elem-es) (immutable-struct-type? (struct-info v)))
                       (const v)]
                      [else `(make-prefab-struct (quote ,key) ,@elem-es)]))]
          [(box? v)
           (let ([e (loop (unbox v))])
             (cond [(const? e) (const v)]
                   [else `(box-immutable ,e)]))]
          [(hash? v)
           (define ctor
             (cond [(hash-equal? v) 'hash]
                   [(hash-equal-always? v) 'hashalw]
                   [(hash-eqv? v) 'hasheqv]
                   [(hash-eq? v) 'hasheq]
                   [else (bad v)]))
           (define kv-exprss
             (for/list ([(k v) (in-hash v)])
               (list (loop k) (loop v))))
           (if (for/and ([kves (in-list kv-exprss)])
                 (and (const? (car kves)) (const? cadr kves)))
               (const v)
               (cons ctor (apply append kv-exprss)))]
          [(convert v)
           => (lambda (result)
                (unless (syntax? result)
                  (error 'datum->expression
                         "result from convert function was not syntax\n  got: ~e"
                         result))
                result)]
          [else (bad v)]))
  (let ([main-expr (loop v0)])
    (datum->syntax #'here
                   `(let ,(for/list ([(expr var) (in-hash syntax-h)])
                            (list var expr))
                        ,main-expr))))
