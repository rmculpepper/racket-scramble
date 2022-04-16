;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/contract/base
         racket/struct-info
         syntax/transformer
         (for-template racket/match))
(provide (contract-out
          [adjust-struct-info
           (->* [struct-info?]
                [#:constructor (or/c #f identifier?)
                 #:match-expander (or/c #f (-> syntax? syntax?))]
                struct-info?)]))

;; ------------------------------------------------------------

(define (adjust-struct-info v
                            #:constructor [new-constructor-id #f]
                            #:match-expander [new-match-expander #f])
  (define si (extract-struct-info v))
  (define constructor-id (or new-constructor-id (cadr si)))
  (define-syntax-rule (listif c e) (if c (list e) null))
  (make-info (let ([v (list* (car si) constructor-id (cddr si))])
               (lambda () v))
             (append
              (listif #t
                      (list prop:procedure 'field-index
                            (make-variable-like-transformer constructor-id)))
              (listif new-match-expander
                      (list prop:match-expander 'field-index new-match-expander))
              (listif (struct-auto-info? v)
                      (list prop:struct-auto-info 'get-field
                            (struct-auto-info-lists v)))
              (listif (struct-field-info? v)
                      (list prop:struct-field-info 'get-field
                            (struct-field-info-list v))))))

;; ------------------------------------------------------------

;; A PropDecl is (list StructTypeProperty (U 'get-field 'field-index) Any)
;; - 'get-field means the property value is an accessor function for the field
;; - 'field-index means the property value is the field index

;; make-info : StructInfoList (Listof PropDecl) -> StructInfo
(define (make-info si props)
  (define type+constructor (get-struct-type (reverse props)))
  (define constructor (cdr type+constructor))
  (apply constructor si (map caddr props)))

;; get-struct-type : (Listof PropDecl) -> (cons StructType Constructor)
;; Gets or creates a struct type that carries the given properties.
(define (get-struct-type props)
  (hash-ref! struct-type-table (map car props)
             (lambda ()
               (cond [(null? props)
                      (cons struct:struct-info-base struct-info-base)]
                     [else
                      (define prop (car props)) ;; (List StructTypeProperty Symbol _)
                      (define super-type (car (get-struct-type (cdr props))))
                      (struct struct-info* (v)
                        #:super super-type
                        #:property (car prop)
                        (case (cadr prop)
                          [(get-field) (lambda (self) (struct-info*-v self))]
                          [(field-index) (struct-field-index v)]
                          [else (error 'prop "bad prop mode: ~e" (cadr prop))]))
                      (cons struct:struct-info* struct-info*)]))))

;; struct-type-table : Hash[(Listof StructTypeProperty) => (cons StructType Constructor)]
;; Note: This assumes that a property is always used with the same mode symbol.
(define struct-type-table (make-hash))

(struct struct-info-base () #:super struct:struct-info)
