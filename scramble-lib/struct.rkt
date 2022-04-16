;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/contract/base
         racket/match
         racket/list)
(provide (contract-out
          [prop:auto-equal+hash
           (struct-type-property/c
            (or/c #t (listof exact-nonnegative-integer?)))]))

;; References:
;; - /racket/racket/src/cs/rumble/hash-code.ss
;; - https://en.wikipedia.org/wiki/List_of_hash_functions
;; - FastHash (2012) (https://github.com/ztanml/fast-hash)
;;   - mix function uses 2 xorshifts and 1 multiply
;; - MurmurHash2
;;   - mix function uses 1 xorshift and 2 multiplies

;; ============================================================
;; Based on FastHash, but with several adjustments, including
;; - truncated constants and intermediates to Racket fixnums
;; - update function extrapolated from original's string processing
;;   - Note: compared to the original (string) function, this loses an
;;     ingredient: the length of the input. The prop:auto-equal+hash wrapper
;;     adds the struct name as an ingredient as partial compensation.

(module private-hash-function racket/base
  (require racket/contract/base
           racket/fixnum)
  (provide fh-init
           fh-init2
           (contract-out
            [fh-update
             (-> fixnum? exact-integer? fixnum?)]
            [fh-final
             (-> fixnum? fixnum?)]))

  (define 64-bit? (fixnum? (expt 2 33)))

  (define fixnum-mask  ;; greatest positive fixnum with bit pattern "0*1+"
    (let loop ([fuel 64] [acc 1])
      (define next (bitwise-ior 1 (arithmetic-shift acc 1)))
      (if (and (positive? fuel) (fixnum? next))
          (loop (sub1 fuel) next)
          acc)))

  (define (->fx v) (if (fixnum? v) v (bitwise-and v fixnum-mask)))

  (define fh-a (->fx #x2127599bf4325c37))
  (define fh-m (->fx #x880355f21e6d1965))

  (define (fh-mix64 h)
    (let* ([h (fxxor h (fxrshift h 23))]
           [h (fx*/wraparound h fh-a)]
           [h (fxxor h (fxrshift h 47))])
      h))

  (define (fh-mix32 h)
    (let* ([h (fxxor h (fxrshift h 11))]
           [h (fx*/wraparound h fh-a)]
           [h (fxxor h (fxrshift h 23))])
      h))

  (define (fh-mix h) (if 64-bit? (fh-mix64 h) (fh-mix32 h)))

  ;; ----------------------------------------

  ;; S (State) = Fixnum
  ;; C (Code)  = Fixnum

  (define init1 (->fx (random (expt 2 24))))
  (define init2 (->fx (random (expt 2 24))))

  ;; fh-init{,2} : -> S
  (define (fh-init) init1)
  (define (fh-init2) init2)

  ;; fh-update : S C -> S
  (define (fh-update h v)
    (let* ([h (fxxor h (fh-mix (->fx v)))]
           [h (fx*/wraparound h fh-m)])
      h))

  ;; fh-final : S -> C
  (define (fh-final h)
    (fh-mix h))

  #;
  (define (fh-combine . vs)
    (fh-final
     (for/fold ([h (fh-init)])
               ([v (in-list vs)])
       (fh-update h v)))))

;; ============================================================

(require (submod "." private-hash-function))

(define-values (prop:auto-equal+hash has-auto-equal+hash? auto-equal+hash-ref)
  (make-struct-type-property
   'auto-equal+hash
   (lambda (v info)
     (define who 'prop:auto-equal+hash)
     (match-define (list name inits autos ref mut imms super skipped?) info)
     (when skipped?
       (error who "struct type's super type is not controlled by current inspector\n  struct: ~.s"
              name))
     (when (and super (not (has-auto-equal+hash? super)))
       (error who (string-append "struct type's super type does not implement prop:auto-equal+hash"
                                 "\n  struct: ~.s\n  super type: ~e")
              name super))
     (define-values (super-equal? super-hash1 super-hash2)
       (cond [super (apply values (auto-equal+hash-ref super))]
             [else (values #f #f #f)]))
     (define fields
       (cond [(eq? v #t)
              (range (+ inits autos))]
             [(list? v)
              (for ([elem (in-list v)])
                (unless (and (exact-nonnegative-integer? elem) (< elem (+ inits autos)))
                  (error who
                         "struct field index out of range\n  given: ~e\n  fields: ~e\n  struct: ~.s"
                         elem (+ inits autos) name)))
              v]
             [else
              (error who "bad property value for struct type\n  expected: ~s\n  struct: ~.s"
                     '(or/c #t (listof exact-nonnegative-integer?))
                     name)]))
     (make-equal+hash name fields ref super-equal? super-hash1 super-hash2))
   (list (cons prop:equal+hash values))))

(define (make-equal+hash name fields ref super-equal? super-hash1 super-hash2)
  (define (equal-proc this that recur)
    (and (if super-equal? (super-equal? this that recur) #t)
         (for/and ([field (in-list fields)])
           (recur (ref this field) (ref that field)))))
  (define (hash-proc this recur)
    (define init-h (if super-hash1 (super-hash1 this recur) (fh-init)))
    (fh-final
     (for/fold ([h (fh-update init-h (recur name))])
               ([field (in-list fields)])
       (fh-update h (recur (ref this field))))))
  (define (hash-proc2 this recur)
    (define init-h (if super-hash2 (super-hash2 this recur) (fh-init2)))
    (fh-final
     (for/fold ([h (fh-update init-h (recur name))])
               ([field (in-list fields)])
       (fh-update h (recur (ref this field))))))
  (list equal-proc hash-proc hash-proc2))
