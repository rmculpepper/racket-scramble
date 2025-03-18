;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/datum
                     syntax/transformer
                     (submod "private/regexp.rkt" ast)
                     (submod "private/regexp.rkt" codegen)
                     (submod "private/regexp.rkt" syntax)))
(provide define-RE rx px)

;; ------------------------------------------------------------
;; Syntax

(begin-for-syntax
  (define ((make-re-transformer mode make-rx make-byte-rx) stx)
    (syntax-parse stx
      [(_ #:byte re:RE ...)
       #`(quote #,(check-RE mode (make-re:cat (datum (re.ast ...))) make-byte-rx))]
      [(_ re:RE ...)
       #`(quote #,(check-RE mode (make-re:cat (datum (re.ast ...))) make-rx))]))

  (define (check-RE mode ast make-*rx)
    (define src (emit-regexp mode ast))
    (define (fail err) (wrong-syntax #f "~a\n  generated: ~a" err src))
    (make-*rx src fail))

  (define (string->byte-pregexp s fail)
    (byte-pregexp (string->bytes/utf-8 s) fail))
  (define (string->byte-regexp s fail)
    (byte-regexp (string->bytes/utf-8 s) fail)))

(define-syntax rx (make-re-transformer 'rx regexp string->byte-regexp))
(define-syntax px (make-re-transformer 'px pregexp string->byte-pregexp))

(define-syntax (define-RE stx)
  (syntax-parse stx
    [(_ name:id #:byte re:RE)
     (define ast (datum re.ast))
     (define px (check-RE 'px ast string->byte-pregexp))
     #`(define-syntax name (RE-binding (quote #,ast) (quote #,px)))]
    [(_ name:id re:RE)
     (define ast (datum re.ast))
     (define px (check-RE 'px ast pregexp))
     #`(define-syntax name (RE-binding (quote #,ast) (quote #,px)))]))
