;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/contract/base)
(provide (contract-out
          [prop:about
           (struct-type-property/c
            (-> any/c string?))])
         has-about?
         about)

(define-values (prop:about has-about? about-ref)
  (make-struct-type-property 'about))

(define (about v)
  (cond [(has-about? v) ((about-ref v) v)]
        [else "(no description)"]))

(define (about-error-line name v)
  (cond [(has-about? v)
         (format "\n  ~a: ~a" name (about v))]
        [else ""]))
