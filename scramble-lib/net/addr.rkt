;; Copyright 2024 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         racket/list
         racket/string
         racket/struct
         "../number.rkt")

;; ========================================
;; IP v4
(provide (struct-out ip4)
         ip4-in/c
         ip4-number?
         ip4-bytes?
         ip4-string?
         ip4-list?
         ip4-ref
         ip4->number
         ip4->bytes
         ip4->string
         ip4->list
         string->ip4
         (struct-out cidr4)
         cidr4-contains?
         cidr4-range)

;; Reference:
;; - https://tools.ietf.org/html/rfc3986#section-3.2.2

;; An IPv4 is (ip4 UInt32)
(struct ip4 (number)
  #:transparent
  #:guard (lambda (v _info) (ip4-guard 'ip4 v))
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (self) 'ip4)
   (lambda (self) (list (ip4->string self)))))

;; ip4-guard : IPv4In -> UInt32
(define (ip4-guard who v)
  (cond [(ip4-number? v) v]
        [(ip4-check-bytes v #t) => values]
        [(ip4-check-string v #t) => values]
        [(ip4-check-list v #t) => values]
        [(ip4? v) (ip4-number v)]
        [else
         (raise-argument-error
          who "(or/c ip4? ip4-number? ip4-bytes? ip4-string? ip4-list?)" v)]))

(define (ip4-number? v) (uint32? v))
(define (ip4-bytes? v) (ip4-check-bytes v #f))
(define (ip4-string? v) (ip4-check-string v #f))
(define (ip4-list? v) (ip4-check-list v #f))

;; An IPv4In is one of
;; - ip4-number? aka UInt32
;; - ip4-bytes?
;; - ip4-string?
;; - ip4-list?
(define (ip4-in/c v)
  (or (ip4? v) (ip4-number? v) (ip4-bytes? v) (ip4-string? v) (ip4-list? v)))

(define (ip4-ref addr n)
  (define an (ip4-guard 'ip4-ref addr))
  (unless (and (exact-nonnegative-integer? n) (< n 4))
    (raise-argument-error 'ip4-ref "(integer-in 0 3)" n))
  (bitwise-bit-field an (* 8 n) (* 8 (add1 n))))

(define (ip4-check-bytes v to-num?)
  (and (bytes? v)
       (= (bytes-length v) 4)
       (if to-num? (integer-bytes->integer v #f #t) #t)))

(define (ip4-check-string v to-num?)
  (and (string? v)
       (let ([v (string->immutable-string v)])
         (and (regexp-match-exact? ip4-approx-rx v)
              (ip4-check-list (map string->number (string-split v ".")) to-num?)))))

(define (ip4-check-list v to-num?)
  (match v
    [(list (? byte? n1) (? byte? n2) (? byte? n3) (? byte? n4))
     (if to-num?
         (bitwise-ior (arithmetic-shift n1 24)
                      (arithmetic-shift n2 16)
                      (arithmetic-shift n3  8)
                      (arithmetic-shift n4  0))
         #t)]
    [else #f]))

(define ip4-approx-rx #px"(?:[0-9]{1,3}(?:[.][0-9]{1,3}){3})")

(define ip4-exact-rx
  (let ([dec-octet-rx "(?:[0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])"])
    (pregexp (format "(?:~a(?:[.]~a){3})" dec-octet-rx dec-octet-rx))))

;; ip4->number : IPv4/IPv4In -> UInt32
(define (ip4->number addr)
  (ip4-guard 'ip4->number addr))

;; ip4->bytes : IPv4 -> Bytes
(define (ip4->bytes addr)
  (bytes->immutable-bytes
   (integer->integer-bytes (ip4-guard 'ip4->bytes addr) 4 #f #t)))

;; ip4->string : IPv4 -> String
(define (ip4->string addr)
  (match (ip4->list (ip4-guard 'ip4->string addr))
    [(list n1 n2 n3 n4)
     (string->immutable-string
      (format "~a.~a.~a.~a" n1 n2 n3 n4))]))

;; ip4->list : IPv4 -> (listof Byte)
(define (ip4->list addr)
  (define n (ip4-guard 'ip4->list addr))
  (list (bitwise-bit-field n 24 32)
        (bitwise-bit-field n 16 24)
        (bitwise-bit-field n  8 16)
        (bitwise-bit-field n  0  8)))

;; string->ip4 : String -> IPv4 or #f
(define (string->ip4 s)
  (unless (string? s) (raise-argument-error 'string->ip4 "string?" s))
  (cond [(ip4-check-string s #t) => ip4]
        [else #f]))

;; ip4-classify-number : UInt32 -> (Listof Symbol)
;; IANA IP4 (https://www.iana.org/assignments/iana-ipv4-special-registry)
#;
(define (ip4-classify-number n)
  (cond [(cidr4-contains* #x00000000 8  n) '(unspecified)] ;; 0.0.0.0/8
        [(cidr4-contains* #x0A000000 8  n) '(private)]     ;; 10.0.0.0/8
        [(cidr4-contains* #x7F000000 8  n) '(loopback)]    ;; 127.0.0.0/8
        [(cidr4-contains* #xAC100000 12 n) '(private)]     ;; 172.16.0.0/12
        [(cidr4-contains* #xC0A80000 16 n) '(private)]     ;; 192.168.0.0/16
        [(cidr4-contains* #xA9FE0000 16 n) '(link-local)]  ;; 169.254.0.0/16
        [(cidr4-contains* #xE0000000 4  n) '(multicast)]   ;; 224.0.0.0/4
        [(cidr4-contains* #xF0000000 4  n) '(reserved)]    ;; 240.0.0.0/4
        [else '()]))

;; ----------------------------------------
;; IPv4 CIDR

(struct cidr4 (address prefix)
  #:transparent
  #:guard
  (lambda (addr prefix _info)
    (define an (ip4-guard 'cidr4 addr))
    (unless (and (exact-nonnegative-integer? prefix) (<= 0 prefix 32))
      (raise-argument-error 'cidr4 "(integer-in 0 32)" prefix))
    (define an* (bitwise-and an (arithmetic-shift -1 (- 32 prefix)))) ;; clear low bits
    (values an* prefix))
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (self) 'cidr4)
   (lambda (self) (list (ip4->string (cidr4-address self)) (cidr4-prefix self)))))

(define (cidr4-contains? cidr addr)
  (match cidr
    [(cidr4 cn prefix)
     (define an (ip4-guard 'cidr4-contains? addr))
     (cidr4-contains* cn prefix an)]
    [_ (raise-argument-error 'cidr4-contains? "cidr4?" cidr)]))

(define (cidr4-contains* cn prefix an)
  (= (bitwise-bit-field cn (- 32 prefix) 32)
     (bitwise-bit-field an (- 32 prefix) 32)))

(define (cidr4-range cidr)
  (match cidr
    [(cidr4 cn prefix)
     (values cn (+ cn -1 (arithmetic-shift 1 prefix)))]
    [_ (raise-argument-error 'cidr4-range "cidr4?" cidr)]))

;; ========================================
;; IP v6
(provide (struct-out ip6)
         ip6-in/c
         ip6-number?
         ip6-bytes?
         ip6-string?
         ip6-list?
         ip6-ref
         ip6->number
         ip6->bytes
         ip6->string
         ip6->list
         string->ip6
         (struct-out cidr6)
         cidr6-contains?
         cidr6-range)

;; Reference:
;; - https://tools.ietf.org/html/rfc3986#section-3.2.2

;; IP6addr = Integer in [0 .. 2^128-1]

(struct ip6 (number)
  #:transparent
  #:guard (lambda (v _info) (ip6-guard 'ip6 v))
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (self) 'ip6)
   (lambda (self) (list (ip6->string self #:elide? #t)))))

;; ip6-guard : Symbol IPv6In -> UInt128
(define (ip6-guard who v)
  (cond [(ip6-number? v) v]
        [(ip6-check-bytes v #t) => values]
        [(ip6-check-string v #t) => values]
        [(ip6-check-list v #t) => values]
        [(ip6? v) (ip6-number v)]
        [else
         (raise-argument-error
          who "(or/c ip6? ip6-number? ip6-bytes? ip6-string? ip6-list?)" v)]))

(define (ip6-number? v) (uint128? v))
(define (ip6-bytes? v) (ip6-check-bytes v #f))
(define (ip6-string? v) (ip6-check-string v #f))
(define (ip6-list? v) (ip6-check-list v #f))

;; An IPv6In is one of
;; - ip6-number? aka UInt128
;; - ip6-bytes?
;; - ip6-string?
;; - ip6-list?
(define (ip6-in/c v)
  (or (ip6? v) (ip6-number? v) (ip6-bytes? v) (ip6-string? v) (ip6-list? v)))

;; ip6-ref : IPv6In -> UInt128
(define (ip6-ref addr n)
  (define an (ip6-guard 'ip6-ref addr))
  (unless (and (exact-nonnegative-integer? n) (< n 8))
    (raise-argument-error 'ip6-ref "(integer-in 0 7)" n))
  (bitwise-bit-field an (* 16 n) (* 16 (add1 n))))

(define (ip6-check-bytes v to-num?)
  (and (bytes? v)
       (= (bytes-length v) 16)
       (if to-num?
           (bitwise-ior
            (arithmetic-shift (integer-bytes->integer v #f #t 8 16) 64)
            (integer-bytes->integer v #f #t 0 8))
           #t)))

(define (ip6-check-string s to-num?)
  (and (string? s)
       (let ([s (string->immutable-string s)])
         (and (regexp-match-exact? ip6-approx-rx s)
              (match (string-split s "::" #:trim? #f)
                [(list chunk)
                 (define-values (len1 v1) (ip6-check-chunk chunk #t to-num?))
                 (and len1 (= len1 8) (if to-num? v1 #t))]
                [(list chunk1 chunk2)
                 (define-values (len1 v1) (ip6-check-chunk chunk1 #f to-num?))
                 (define-values (len2 v2) (ip6-check-chunk chunk2 #t to-num?))
                 (and len1 len2 (< (+ len1 len2) 8)
                      (if to-num?
                          (bitwise-ior
                           (arithmetic-shift v1 (* 16 (- 8 len1)))
                           v2)
                          #t))]
                [_ #f])))))

(define (ip6-check-chunk chunk final? to-num?)
  (define parts (string-split chunk ":" #:trim? #f))
  (let loop ([parts parts] [len 0] [acc (and to-num? 0)])
    (match parts
      ['()
       (values len acc)]
      [(list (app string->ip4 (ip4 n)))
       #:when final?
       (values (+ len 2) (and to-num? (bitwise-ior (arithmetic-shift acc 32) n)))]
      [(cons part parts)
       (define n (string->number part 16))
       (if n
           (loop parts (+ len 1) (and to-num? (bitwise-ior (arithmetic-shift acc 16) n)))
           (values #f #f))])))

(define (ip6-check-list ns to-num?)
  (and (list? ns)
       (= (length ns) 8)
       (andmap uint16? ns)
       (for/fold ([acc 0]) ([n (in-list ns)])
         (bitwise-ior n (arithmetic-shift acc 16)))))

(define ip6-approx-rx
  (let* ([h16 "[0-9a-fA-F]{1,4}"]
         [h16: (format "(?:~a:)" h16)]
         [end (format "(?:~a|~a)" (object-name ip4-approx-rx) h16)])
    (pregexp
     (format "(?:(?:~a){0,7}~a)?(?:::(?:(?:~a){0,7}(~a))?)?" h16: h16 h16: end))))

(define ip6-exact-rx
  (let* ([h16 "[0-9a-fA-F]{1,4}"]
         [h16: (format "(?:~a:)" h16)]
         [ls32 (format "(?:~a:~a|~a)" h16 h16 (object-name ip4-exact-rx))])
    (pregexp
     (string-join
      (list (format "(?:~a){6}~a" h16: ls32)
            (format "::(?:~a){5}~a" h16: ls32)
            (format "(?:~a{0,0}~a)?::(?:~a){4}~a" h16: h16 h16: ls32)
            (format "(?:~a{0,1}~a)?::(?:~a){3}~a" h16: h16 h16: ls32)
            (format "(?:~a{0,2}~a)?::(?:~a){2}~a" h16: h16 h16: ls32)
            (format "(?:~a{0,3}~a)?::(?:~a){1}~a" h16: h16 h16: ls32)
            (format "(?:~a{0,4}~a)?::(?:~a){0}~a" h16: h16 h16: ls32)
            (format "(?:~a{0,5}~a)?::~a" h16: h16 h16)
            (format "(?:~a{0,6}~a)?::" h16: h16))
      "|"))))

;; ip6->number : IPv6 -> UInt128
(define (ip6->number addr)
  (ip6-guard 'ip6->number addr))

;; ip6->bytes : IPv4 -> Bytes
(define (ip6->bytes addr)
  (define n (ip6-guard 'ip6->bytes addr))
  (bytes->immutable-bytes
   (bytes-append
    (integer->integer-bytes (bitwise-bit-field n 64 128) 16 #f #t)
    (integer->integer-bytes (bitwise-bit-field n 0  64)  16 #f #t))))

;; ip6->list : IPv6 -> (Listof UInt16)
(define (ip6->list addr)
  (define n (ip6-guard 'ip6->list addr))
  (list (bitwise-bit-field n 112 128)
        (bitwise-bit-field n 96  112)
        (bitwise-bit-field n 80  96)
        (bitwise-bit-field n 64  80)
        (bitwise-bit-field n 48  64)
        (bitwise-bit-field n 32  48)
        (bitwise-bit-field n 16  32)
        (bitwise-bit-field n 0   16)))

;; ip6->string : IPv6 -> String
(define (ip6->string addr
                     #:elide? [elide? #t])
  (define ns (ip6->list (ip6-guard 'ip6->string addr)))
  (string->immutable-string
   (if elide?
       (ip6-parts->string/elide ns)
       (string-join (for/list ([n (in-list ns)]) (number->string n 16)) ":"))))

;; ip6-parts->string/elide : IP6Addr -> String
(define (ip6-parts->string/elide ns)
  (define-values (skip-index suffix)
    (for/fold ([best-index -1] [best-len 0] [best-suffix null] [tail ns]
               #:result (values best-index best-suffix))
              ([index (in-range (length ns))])
      (define-values (this-skip this-suffix)
        (let loop ([tail tail] [acc 0])
          (if (and (pair? tail) (zero? (car tail)))
              (loop (cdr tail) (add1 acc))
              (values acc tail))))
      (cond [(> this-skip best-len)
             (values index this-skip this-suffix (cdr tail))]
            [else
             (values best-index best-len best-suffix (cdr tail))])))
  ;; If no skips, best-index is -1.
  (define (to-hex n) (number->string n 16))
  (define (loop parts index)
    (cond [(= index skip-index)
           (cons "::" (add-between (map to-hex suffix) ":"))]
          [(pair? parts)
           (list* (if (zero? index) "" ":")
                  (to-hex (car parts))
                  (loop (cdr parts) (add1 index)))]
          [else null]))
  (apply string-append (loop ns 0)))

;; string->ip6 : String -> IPv6 / #f
(define (string->ip6 s)
  (unless (string? s) (raise-argument-error 'string->ip6 "string?" s))
  (cond [(ip6-check-string s #t) => ip6]
        [else #f]))

;; ip6-classify-number : UInt32 -> (Listof Symbol)
;; IANA IP6 (https://www.iana.org/assignments/iana-ipv6-special-registry)
#;
(define (ip6-classify-number n)
  (cond [(= n 0) '(unspecified)]
        [(= n 1) '(loopback)]
        [(same-prefix 16 #xFC00 128 n 7) '(unique-local)]
        [(same-prefix 16 #xFE80 128 n 10) '(link-local)]
        [(same-prefix 16 #xFF00 128 n 8) '(multicast)]
        [else '()]))
#;
(define (same-prefix clen cn alen an pfx)
  (= (bitwise-bit-field cn (- clen pfx) clen)
     (bitwise-bit-field an (- alen pfx) alen)))

;; ----------------------------------------
;; IPv6 CIDR

(struct cidr6 (address prefix)
  #:transparent
  #:guard
  (lambda (addr prefix _info)
    (define an (ip6-guard 'cidr6 addr))
    (unless (and (exact-nonnegative-integer? prefix) (<= 0 prefix 128))
      (raise-argument-error 'cidr6 "(integer-in 0 128)" prefix))
    (define an* (bitwise-and an (arithmetic-shift -1 (- 128 prefix)))) ;; clear low bits 
    (values an* prefix))
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (self) 'cidr6)
   (lambda (self) (list (ip6->string (cidr6-address self)) (cidr6-prefix self)))))

(define (cidr6-contains? cidr addr)
  (match cidr
    [(cidr6 cn prefix)
     (define an (ip6-guard 'cidr6-contains? addr))
     (cidr6-contains* cn prefix an)]
    [_ (raise-argument-error 'cidr6-contains? "cidr6?" cidr)]))

(define (cidr6-contains* cn prefix an)
  (= (bitwise-bit-field cn (- 128 prefix) 128)
     (bitwise-bit-field an (- 128 prefix) 128)))

(define (cidr6-range cidr)
  (match cidr
    [(cidr6 cn prefix)
     (values cn (+ cn -1 (arithmetic-shift 1 prefix)))]
    [_ (raise-argument-error 'cidr6-range "cidr6?" cidr)]))
