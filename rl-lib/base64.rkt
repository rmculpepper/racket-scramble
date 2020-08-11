;; Copyright 2018-2020 Ryan Culpepper

#lang racket/base
(require "list.rkt")
(provide base64-encode
         base64-decode)

;; References:
;; - https://en.wikipedia.org/wiki/Base64

;; ----------------------------------------

(define b64-prefix #"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
(define std-endcodes #"+/")
(define alt-endcodes #"./")
(define url-endcodes #"-_")

;; encode1 : Bytes[2] Nat[0..63] -> Byte
(define (encode1 endcodes k)
  (cond [(< k 62) (bytes-ref b64-prefix k)]
        [(< k 64) (bytes-ref endcodes (- k 62))]))

;; decode1 : Bytes[2] Byte -> Nat[0..63]
(define (decode1 endcodes n #:who [who 'decode1])
  (cond [(<= (char->integer #\A) n (char->integer #\Z))
         (+ 0 (- n (char->integer #\A)))]
        [(<= (char->integer #\a) n (char->integer #\z))
         (+ 26 (- n (char->integer #\a)))]
        [(<= (char->integer #\0) n (char->integer #\9))
         (+ 52 (- n (char->integer #\0)))]
        [(= n (bytes-ref endcodes 0)) 62]
        [(= n (bytes-ref endcodes 1)) 63]
        [else (error who "bad base64(~a) code: ~e" endcodes n)]))

;; ----------------------------------------

;; encode : Bytes Bytes[2] Boolean -> Bytes
(define (encode src endcodes pad?)
  (define (code k) (encode1 endcodes k))
  (define srclen (bytes-length src))
  (define outlen
    (+ (* 4 (quotient srclen 3))
       (if pad?
           (case (remainder srclen 3) [(0) 0] [else 4])
           (case (remainder srclen 3) [(0) 0] [(1) 2] [(2) 3]))))
  (define out (make-bytes outlen))
  (for ([srci (in-range 0 srclen 3)]
        [outi (in-range 0 outlen 4)])
    (define n (read-triplet src srci srclen))
    (write-quad out outi outlen n code))
  (when pad?
    (define padlen (case (remainder srclen 3) [(0) 0] [(1) 2] [(2) 1]))
    (for ([i (in-range (- outlen padlen) outlen)])
      (bytes-set! out i (char->integer #\=))))
  out)

(define (read-triplet src srci srclen)
  (define (get srci) (if (< srci srclen) (bytes-ref src srci) 0))
  (+ (arithmetic-shift (get (+ srci 0)) 16)
     (arithmetic-shift (get (+ srci 1)) 8)
     (get (+ srci 2))))

(define (write-quad out outi outlen n code)
  (define (put outi v) (when (< outi outlen) (bytes-set! out outi v)))
  (put (+ outi 0) (code (bitwise-bit-field n 18 24)))
  (put (+ outi 1) (code (bitwise-bit-field n 12 18)))
  (put (+ outi 2) (code (bitwise-bit-field n 6  12)))
  (put (+ outi 3) (code (bitwise-bit-field n 0  6))))

;; ----------------------------------------

;; decode : Bytes Bytes[2] -> Bytes
(define (decode src endcodes #:who who)
  (define (dc k) (decode1 endcodes k #:who who))
  (define srclen (bytes-length src))
  (define outlen
    (+ (* 3 (quotient srclen 4))
       (case (remainder srclen 4) [(0) 0] [(2) 1] [(3) 2])))
  (define out (make-bytes outlen))
  ;; Decode main part (full quads)
  (for ([srci (in-range 0 srclen 4)]
        [outi (in-range 0 outlen 3)])
    (define n (read-quad src srci srclen dc))
    (write-triplet out outi outlen n))
  out)

(define (read-quad src srci srclen dc)
  (define (get srci) (if (< srci srclen) (dc (bytes-ref src srci)) 0))
  (+ (arithmetic-shift (get (+ srci 0)) 18)
     (arithmetic-shift (get (+ srci 1)) 12)
     (arithmetic-shift (get (+ srci 2))  6)
     (get (+ srci 3))))

(define (write-triplet out outi outlen n)
  (define (put outi v) (when (< outi outlen) (bytes-set! out outi v)))
  (put (+ outi 0) (bitwise-bit-field n 16 24))
  (put (+ outi 1) (bitwise-bit-field n 8  16))
  (put (+ outi 2) (bitwise-bit-field n 0  8)))

;; ----------------------------------------

;; coerce-src : Symbol (U Bytes String) -> Bytes
(define (coerce-src who src)
  (cond [(bytes? src) src]
        [(string? src) (string->bytes/utf-8 src)]
        [else (raise-argument-error who "(or/c string? bytes?)" src)]))

(define (coerce-endcodes who v)
  (cond [(and (bytes? v) (= 2 (bytes-length v))) v]
        [else (error who "endcodes is not a byte string of length 2\n  endcodes: ~e" v)]))

(define (get-content-rx endcodes)
  (cond [(equal? endcodes std-endcodes)
         #rx#"[A-Za-z0-9+/]+"]
        [(equal? endcodes alt-endcodes)
         #rx#"[A-Za-z0-9./]+"]
        [(equal? endcodes url-endcodes)
         #rx#"[A-Za-z0-9_-]+$"]
        [else
         (byte-regexp (format "(?:[A-Za-z0-9]|~a|~a)+"
                              (regexp-quote (bytes (bytes-ref endcodes 0)))
                              (regexp-quote (bytes (bytes-ref endcodes 1)))))]))

;; ============================================================

(define (base64-encode src0 [endcodes0 std-endcodes]
                       #:line [line #f] #:line-sep [line-sep #"\r\n"]
                       #:pad? [pad? #f] #:who [who 'base64-encode])
  (define endcodes (coerce-endcodes who endcodes0))
  (define (do-enc bs) (encode bs endcodes pad?))
  (cond [line
         (unless (and (exact-positive-integer? line) (zero? (remainder line 4)))
           (error who "line length not a multiple of 4\n  line: ~e" line))
         (unless (regexp-match? #px#"^[[:space:]]*$" line-sep)
           (error who "line separator contains non-whitespace\n  separator: ~e" line-sep))
         (define in-line (* 3 (quotient line 4)))
         (define src (coerce-src who src0))
         (define srclen (bytes-length src))
         (define out (open-output-bytes))
         (for ([start (in-range 0 srclen in-line)])
           (write-bytes (do-enc (subbytes src start (min srclen (+ start in-line)))) out)
           (write-bytes line-sep out))
         (get-output-bytes out)]
        [else (do-enc (coerce-src who src0))]))

(define (base64-decode src0 [endcodes0 std-endcodes]
                       #:mode [mode 'whitespace] #:who [who 'base64-decode])
  (define endcodes (coerce-endcodes who endcodes0))
  (define src (coerce-src who src0))
  (define content-rx (get-content-rx endcodes))
  (define (do-dec bs) (decode bs endcodes #:who who))
  (define content-ranges (regexp-match-positions* content-rx src))
  (case mode
    [(strict)
     (cond [(and (singleton? content-ranges)
                 (let ([r (car content-ranges)])
                   (and (= 0 (car r)) (= (bytes-length src) (cdr r)))))
            (do-dec src)]
           [else (error who "bad base64 encoding")])]
    [else
     (case mode
       [(whitespace)
        (for/fold ([start 0]) ([r (in-list content-ranges)])
          (unless (regexp-match? #px#"^[=[:space:]]*$" src start (car r))
            (error who "bad base64 encoding"))
          (cdr r))]
       [else (void)])
     (define out (open-output-bytes))
     (for ([r (in-list content-ranges)])
       (write-bytes (do-dec (subbytes src (car r) (cdr r))) out))
     (get-output-bytes out)]))
