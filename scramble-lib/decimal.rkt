;; Copyright 2024 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         racket/serialize
         racket/math)
(provide (struct-out decimal)
         real->decimal
         string->decimal
         decimal->string
         decimal->exact
         decimal->inexact
         decimal-adjust
         decimal+
         decimal-
         decimal*
         decimal-cmp
         decimal<?
         decimal<=?
         decimal=?
         decimal>?
         decimal>=?
         decimal-min
         decimal-max
         decimal-abs)

;; ============================================================
;; Raw Decimals

(module rawdecimal racket/base
  (require racket/math)
  (provide string->rawdecimal
           rawdecimal->string
           rawdecimal->exact
           rawdecimal->inexact
           rawdecimal-adjust
           rawdecimal-full-add
           rawdecimal-full-mul)

  ;; RawDecimal[N] = Integer
  ;; eg, RawDecimal[6] represents 123.456 as 123456000

  ;; string->rawdecimal : String -> (values RawDecimal[N] N)
  (define (string->rawdecimal s)
    (cond [(regexp-match #rx"^(-?[0-9]+)(#*)[.]?$" s)
           => (lambda (m)
                (values (string->number (cadr m))
                        (- (string-length (caddr m)))))]
          [(regexp-match #rx"^(-?[0-9]*)[.]([0-9]+)$" s)
           => (lambda (m)
                (define whole-s (cadr m))
                (define frac-s (caddr m))
                (values (string->number (string-append whole-s frac-s))
                        (string-length frac-s)))]
          [else (values #f #f)]))

  ;; rawdecimal->string : RawDecimal[N] Nat -> String
  (define (rawdecimal->string n prec)
    (cond [(> prec 0)
           (define n* (abs n))
           (define shift (expt 10 prec))
           (define prefix (if (negative? n) "-" ""))
           (define whole (quotient n* shift))
           (define frac (remainder n* shift))
           (define whole-s (number->string whole))
           (define frac-s (number->string frac))
           (define frac-pad-s (make-string (- prec (string-length frac-s)) #\0))
           (string-append prefix whole-s "." frac-pad-s frac-s)]
          [(zero? prec) (number->string n)]
          [else ;; extension: negative precision
           (string-append (number->string n)
                          (make-string (- prec) #\#))]))

  ;; ----------------------------------------

  ;; rawdecimal->exact : RawDecimal[N] N:Nat -> ExactReal
  (define (rawdecimal->exact n prec)
    (/ n (expt 10 prec)))

  ;; rawdecimal->inexact : RawDecimal[N] N:Nat -> Real
  (define (rawdecimal->inexact n prec)
    (/ (exact->inexact n) (expt 10.0 prec)))

  ;; rawdecimal-adjust : RawDecimal[N] N:Nat P:Nat -> RawDecimal[P]
  (define (rawdecimal-adjust n prec to-prec)
    (cond [(= prec to-prec) n]
          [(< prec to-prec) (* n (expt 10 (- to-prec prec)))]
          [(> prec to-prec)
           (cond [(zero? n) 0]
                 [(negative? n) (- (rawdecimal-adjust-down (abs n) prec to-prec))]
                 [else (rawdecimal-adjust-down n prec to-prec)])]))

  (define (rawdecimal-adjust-down n prec to-prec)
    ;; PRE: n is positive, prec > to-prec
    (define shift (expt 10 (- prec to-prec)))
    (define q (quotient n shift))
    (define rr (* 2 (remainder n shift)))
    (+ q (cond [(< rr shift) 0]
               [(> rr shift) 1]
               ;; else round to even
               [(even? q) 0]
               [else 1])))

  ;; ----------------------------------------
  ;; Arithmetic with full precision

  (define (rawdecimal-full-add n1 prec1 n2 prec2)
    (if (= prec1 prec2)
        (values (+ n1 n2) prec1)
        (let ([maxprec (max prec1 prec2)])
          (values (+ (rawdecimal-adjust n1 prec1 maxprec)
                     (rawdecimal-adjust n2 prec2 maxprec))
                  maxprec))))

  (define (rawdecimal-full-mul n1 prec1 n2 prec2)
    (values (* n1 n2) (+ prec1 prec2)))

  (void))

(require (submod "." rawdecimal))

;; ============================================================
;; Decimals

;; PRINT-PREC-LIMIT : Nat
;; If (abs precision) under this, print decimal, else "<n>E<-p>".
(define PRINT-PREC-LIMIT 10)

(serializable-struct decimal (unscaled scale)
  ;; Note: "scale" is usually called "precision" in the code
  #:transparent
  #:guard (lambda (unscaled precision name)
            (unless (exact-integer? unscaled)
              (raise-argument-error name "exact-integer?" 0 unscaled precision))
            (unless (exact-integer? precision)
              (raise-argument-error name "exact-integer?" 1 unscaled precision))
            (values unscaled precision))
  #:property prop:custom-write
  (lambda (self out mode)
    (match-define (decimal n prec) self)
    (cond [(< (abs prec) PRINT-PREC-LIMIT)
           (fprintf out "#<decimal:~a>" (decimal->string self))]
          [else
           (fprintf out "#<decimal:~aE~a>" n (- prec))])))

(define (check-decimal who v [vs null])
  (define (check v)
    (unless (decimal? v)
      (raise-argument-error who "decimal?" v)))
  (check v)
  (for ([v (in-list vs)]) (check v)))

(define (check-real/decimal who v [vs null])
  (define (check v)
    (unless (or (decimal? v) (real? v))
      (raise-argument-error who "(or/c decimal? real?)" v)))
  (check v)
  (for ([v (in-list vs)]) (check v)))

;; ----------------------------------------

;; real->decimal : Rational N:Integer -> Decimal[N]
(define (real->decimal x prec)
  (unless (rational? x)
    (raise-argument-error 'real->decimal "rational?" 0 x prec))
  (unless (exact-integer? prec)
    (raise-argument-error 'real->decimal "exact-integer?" 1 x prec))
  (define n0 (* x (expt 10 prec))) ;; might overflow!
  (cond [(rational? n0) (decimal (exact-round n0) prec)]
        [else (decimal (round (* (inexact->exact x) (expt 10 prec))) prec)]))

;; string->decimal : String -> Decimal/#f
(define (string->decimal s)
  (unless (string? s) (raise-argument-error 'string->decimal "string?" s))
  (define-values (n prec) (string->rawdecimal s))
  (if n (decimal n prec) #f))

;; decimal->string : Decimal -> String
(define (decimal->string dec)
  (match dec
    [(decimal n prec) (rawdecimal->string n prec)]
    [_ (check-decimal 'decimal->string dec)]))

;; decimal->exact : (U Decimal Rational) -> ExactRational
(define (decimal->exact dec)
  (match dec
    [(decimal n prec) (/ n (expt 10 prec))]
    [(? rational? x) (inexact->exact x)]
    [_ (raise-argument-error 'decimal->exact "(or/c decimal? rational?)" dec)]))

;; decimal->inexact : (U Decimal Real) -> Real
(define (decimal->inexact dec)
  (match dec
    [(decimal n prec) (/ (exact->inexact n) (expt 10.0 prec))]
    [(? real? x) (exact->inexact x)]
    [_ (check-real/decimal 'decimal->inexact dec)]))

;; decimal-adjust : Decimal[N] N:Nat P:Nat -> Decimal[P]
(define (decimal-adjust dec to-prec)
  (match dec
    [(decimal n prec)
     (decimal (rawdecimal-adjust n prec to-prec) to-prec)]
    [_ (check-decimal 'decimal-adjust dec)]))

;; ----------------------------------------
;; Arithmetic

;; Allows mixture of decimal and real values.
;; May produce special inexacts (eg, +inf.0) due to inexact overflow, etc.

(define (mkdecimal n prec)
  (cond [(exact-integer? n) (decimal n prec)]
        [(rational? n) (decimal (exact-round n) prec)]
        [else n]))

(define (decimal-add v1 v2)
  (match v1
    [(decimal n1 prec1)
     (match v2
       [(decimal n2 prec2)
        (define-values (n prec) (rawdecimal-full-add n1 prec1 n2 prec2))
        (decimal n prec)]
       [0 v1]
       [(? real? x2)
        (mkdecimal (+ n1 (* x2 (expt 10 prec1))) prec1)])]
    [0 v2]
    [(? real? x1)
     (match v2
       [(decimal n2 prec2)
        (mkdecimal (+ n2 (* x1 (expt 10 prec2))) prec2)] 
       [(? real? x2)
        (+ x1 x2)])]))

(define (decimal-sub dec1 v2)
  (match v2
    [(decimal n2 prec2) (decimal-add dec1 (decimal (- n2) prec2))]
    [(? real? x2) (decimal-add dec1 (- x2))]))

(define (decimal-mul v1 v2)
  (match v1
    [(decimal n1 prec1)
     (match v2
       [(decimal n2 prec2)
        (define-values (n prec) (rawdecimal-full-mul n1 prec1 n2 prec2))
        (decimal n prec)]
       [1 v2]
       [(? real? x2)
        (mkdecimal (* n1 x2) prec1)])]
    [1 v2]
    [(? real? x1)
     (match v2
       [(decimal n2 prec2)
        (mkdecimal (* x1 n2) prec2)]
       [(? real? x2)
        (* x1 x2)])]))

(define (decimal-cmp v1 v2)
  (decimal-cmp* 'decimal-cmp v1 v2))

(define (decimal-cmp* who v1 v2)
  (define (cmp n1 n2)
    (cond [(= n1 n2) '=]
          [(< n1 n2) '<]
          [(> n1 n2) '>]
          [else #f]))
  (match v1
    [(decimal n1 prec1)
     (match v2
       [(decimal n2 prec2)
        (define maxprec (max prec1 prec2))
        (cmp (rawdecimal-adjust n1 prec1 maxprec)
             (rawdecimal-adjust n2 prec2 maxprec))]
       [(? real? x2)
        (cond [(zero? x2) (cmp n1 0)]
              [(exact? x2) (cmp n1 (* x2 (expt 10 prec1)))]
              [else (cmp (rawdecimal->exact n1 prec1) x2)])]
       [_ (check-real/decimal who v2)])]
    [(? real? x1)
     (match v2
       [(decimal n2 prec2)
        (cond [(zero? x1) (cmp 0 n2)]
              [(exact? x1) (cmp (* x1 (expt 10 prec2)) n2)]
              [else (cmp x1 (rawdecimal->exact n2 prec2))])]
       [(? real? x2)
        (cmp x1 x2)]
       [_ (check-real/decimal who v2)])]
    [_ (check-real/decimal who v1)]))

;; WARNING: decimal{+,-,*} are neither associative nor commutative when mixing
;; decimals (especially of increasing precision) and scalars.

(define decimal+
  (case-lambda
    [() 0]
    [(v)
     (check-real/decimal 'decimal+ v)
     v]
    [(v1 v2)
     (check-real/decimal 'decimal+ v1)
     (check-real/decimal 'decimal+ v2)
     (decimal-add v1 v2)]
    [(v1 . vs)
     (check-real/decimal 'decimal+ v1 vs)
     (for/fold ([d v1]) ([v (in-list vs)]) (decimal-add d v))]))

(define decimal-
  (case-lambda
    [(v)
     (match v
       [(decimal n prec) (decimal (- n) prec)]
       [(? real? x) (- x)]
       [_ (check-real/decimal 'decimal- v)])]
    [(v1 v2)
     (check-real/decimal 'decimal- v1)
     (check-real/decimal 'decimal- v2)
     (decimal-sub v1 v2)]
    [(v1 . vs)
     (check-real/decimal 'decimal- v1)
     (for ([v (in-list vs)]) (check-real/decimal 'decimal- v))
     (for/fold ([d v1]) ([v (in-list vs)]) (decimal-sub d v))]))

(define decimal*
  (case-lambda
    [() 1]
    [(v)
     (check-real/decimal 'decimal* v)
     v]
    [(v1 v2)
     (check-real/decimal 'decimal* v1)
     (check-real/decimal 'decimal* v2)
     (decimal-mul v1 v2)]
    [(v1 . vs)
     (check-real/decimal 'decimal* v1 vs)
     (for/fold ([d v1]) ([v (in-list vs)]) (decimal-mul d v))]))

;; ----------------------------------------
;; Comparison

;; Allows mixture of decimal and real values.

(define (chain-cmp who oks v1 vs)
  (and (memq (decimal-cmp* who v1 (car vs)) oks)
       (for/and ([vv1 (in-list vs)]
                 [vv2 (in-list (cdr vs))])
         (memq (decimal-cmp* who vv1 vv2) oks))))

(define-syntax-rule (cmp-fun who (ok ...))
  (case-lambda
    [(v1) #t]
    [(v1 v2) (let ([r (decimal-cmp* who v1 v2)]) (or (eq? r (quote ok)) ...))]
    [(v1 . vs) (and (chain-cmp who '(ok ...) v1 vs) #t)]))

(define decimal<?  (cmp-fun 'decimal<?  (<)))
(define decimal<=? (cmp-fun 'decimal<=? (< =)))
(define decimal=?  (cmp-fun 'decimal=?  (=)))
(define decimal>?  (cmp-fun 'decimal>?  (>)))
(define decimal>=? (cmp-fun 'decimal>=? (> =)))

(define (decimal-min v1 . vs)
  (check-real/decimal 'decimal-min v1 vs)
  (for/fold ([m v1]) ([v (in-list vs)])
    (if (decimal<? v m) v m)))
(define (decimal-max v1 . vs)
  (check-real/decimal 'decimal-max v1 vs)
  (for/fold ([m v1]) ([v (in-list vs)])
    (if (decimal>? v m) v m)))

(define (decimal-abs dec)
  (match dec
    [(decimal n prec) (if (negative? n) (decimal (abs n) prec) dec)]
    [_ (check-decimal 'decimal-abs dec)]))

;; ----------------------------------------
;; Other operations

(define (decimal-zero? d)
  (match d
    [(decimal n _) (zero? n)]
    [_ (check-decimal 'decimal-zero? d)]))
(define (decimal-negative? d)
  (match d
    [(decimal n _) (negative? n)]
    [_ (check-decimal 'decimal-negative? d)]))
(define (decimal-positive? d)
  (match d
    [(decimal n _) (positive? n)]
    [_ (check-decimal 'decimal-positive? d)]))
