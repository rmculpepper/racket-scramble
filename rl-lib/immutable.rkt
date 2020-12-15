#lang racket/base
(require racket/generic)
(provide immutable
         convertible-to-immutable?
         gen:convertible-to-immutable

         mutable
         convertible-to-mutable?
         gen:convertible-to-mutable)

;; Note: mutable/immutable is a shallow property. For example, an immutable
;; vector might contain mutable strings; the vector is still considered
;; immutable.

;; This library does not convert between (immutable) pairs and mutable
;; pairs. There are two main reasons:
;; 1. The two types don't share an interface (eg, can't call car on a mpair).
;; 2. Converting only one pair would be useless; converting a list presents
;;    problems (eg improper or cyclic lists).

;; ============================================================

;; immutable : X -> Immutable-X
;; If the argument is already immutable, just return it. That is, not guaranteed
;; to get a fresh object.

(define (hash->immutable-hash h)
  (cond [(immutable? h) h]
        #;
        [else
         (define init-h
           (cond [(hash-eq? h) '#hasheq()]
                 [(hash-eqv? h) '#hasheqv()]
                 [(hash-equal? h) '#hash()]
                 [else (error 'hash->immutable-hash "unknown hash type: ~e" h)]))
         (for/fold ([hh init-h]) ([(k v) (in-hash h)]) (hash-set hh k v))]
        [else
         (define maker
           (cond [(hash-eq? h) make-immutable-hasheq]
                 [(hash-eqv? h) make-immutable-hasheqv]
                 [(hash-equal? h) make-immutable-hash]
                 [else (error 'hash->immutable-hash "unknown hash type: ~e" h)]))
         (maker (for/list ([(k v) (in-hash h)]) (cons k v)))]))

(define (box->immutable-box b)
  (box-immutable (unbox b)))

(define-generics convertible-to-immutable
  #:fast-defaults
  ([string?
    (define immutable string->immutable-string)]
   [bytes?
    (define immutable bytes->immutable-bytes)]
   [vector?
    (define immutable vector->immutable-vector)]
   [hash?
    (define immutable hash->immutable-hash)]
   [box?
    (define immutable box->immutable-box)])
  ;; Methods
  (immutable convertible-to-immutable)
  #:defaults
  ([extended-convertible-to-immutable?
    (define immutable extended-convert-to-immutable)]))

;; ============================================================

;; mutable : X [Boolean] -> Mutable-X
;; Returns a mutable version of the argument. If fresh? is true, then shallow
;; mutatation of the result should not affect the argument. If fresh? is false
;; and the argument is mutable, it MAY simply return the argument.

(define (string->mutable-string s [fresh? #t])
  (if (or (immutable? s) fresh?) (string-copy s) s))
(define (bytes->mutable-bytes s [fresh? #t])
  (if (or (immutable? s) fresh?) (bytes-copy s) s))
(define (vector->mutable-vector v [fresh? #t])
  (if (or (immutable? v) fresh?)
      (let ([vv (make-vector (vector-length v))])
        (vector-copy! vv 0 v 0)
        vv)
      v))
(define (box->mutable-box b [fresh? #t])
  (if (or (immutable? b) fresh?) (box (unbox b)) b))

(define (hash->mutable-hash h [fresh? #t])
  (cond [(and (not (immutable? h)) (not fresh?)) h]
        [else
         (define hh
           (cond [(hash-weak? h)
                  (cond [(hash-eq? h) (make-weak-hasheq)]
                        [(hash-eqv? h) (make-weak-hasheqv)]
                        [(hash-equal? h) (make-weak-hash)]
                        [else (error 'hash->mutable-hash "unknown hash type: ~e" h)])]
                 [else
                  (cond [(hash-eq? h) (make-hasheq)]
                        [(hash-eqv? h) (make-hasheqv)]
                        [(hash-equal? h) (make-hash)]
                        [else (error 'hash->mutable-hash "unknown hash type: ~e" h)])]))
         (for ([(k v) (in-hash h)]) (hash-set! hh k v))
         hh]))

(define-generics convertible-to-mutable
  #:fast-defaults
  ([string?
    (define mutable string->mutable-string)]
   [bytes?
    (define mutable bytes->mutable-bytes)]
   [vector?
    (define mutable vector->mutable-vector)]
   [hash?
    (define mutable hash->mutable-hash)]
   [box?
    (define mutable box->mutable-box)])
  ;; Methods
  (mutable convertible-to-mutable [fresh?])
  #:defaults
  ([extended-convertible-to-mutable?
    (define (mutable self [fresh? #t]) (extended-convert-to-mutable self fresh?))]))

;; ============================================================

(module private-extension racket/base
  (provide (protect-out (all-defined-out)))

  (define extended-conversions-to-immutable null) ;; mutated
  (define (register-conversion-to-immutable! pred convert)
    (set! extended-conversions-to-immutable
          (cons (cons pred convert) extended-conversions-to-immutable)))
  (define (extended-convertible-to-immutable? v)
    (for/or ([e (in-list extended-conversions-to-immutable)])
      ((car e) v)))
  (define (extended-convert-to-immutable v)
    ((for/first ([e (in-list extended-conversions-to-immutable)] #:when (car e)) (cdr e)) v))

  (define extended-conversions-to-mutable null) ;; mutated
  (define (register-conversion-to-mutable! pred convert)
    (set! extended-conversions-to-mutable
          (cons (cons pred convert) extended-conversions-to-mutable)))
  (define (extended-convertible-to-mutable? v)
    (for/or ([e (in-list extended-conversions-to-mutable)])
      ((car e) v)))
  (define (extended-convert-to-mutable v [fresh? #t])
    (define convert
      (for/first ([e (in-list extended-conversions-to-mutable)] #:when (car e)) (cdr e)))
    (convert v fresh?)))
(require (submod "." private-extension))

(module unsafe racket/base
  (require (submod ".." private-extension))
  (provide register-conversion-to-immutable!
           register-conversion-to-mutable!))
