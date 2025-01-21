;; Copyright 2024 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

;; Parts of this module are based on code from json/main from package base.

#lang racket/base
(require racket/match
         racket/string
         racket/symbol)
(provide (all-defined-out))

;; ============================================================
;; Write

(define (write-json+ initial-x [out (current-output-port)]
                     #:convert [convert #f]
                     #:encode [encode 'control]
                     #:who [who 'write-json+])
  ;; write-jsval : Any -> Void
  (define (write-jsval x)
    (cond [convert (write-jsval* (convert x) #t x)]
          [else (write-jsval* x #f x)]))
  ;; write-jsval* : Shallow-JSExpr Boolean Any -> _
  (define (write-jsval* x cnv? orig)
    (cond [(or (exact-integer? x) (inexact-rational? x)) (write x out)]
          [(eq? x #f) (write-bytes #"false" out)]
          [(eq? x #t) (write-bytes #"true" out)]
          [(eq? x 'null) (write-bytes #"null" out)]
          [(string? x) (write-json-string x out encode)]
          [(list? x) (write-json-array x)]
          [(hash? x) (write-json-object x cnv? orig)]
          [(and cnv? (unquoted-printing-string? x))
           (write-string (unquoted-printing-string-value x) out)]
          [else (err:bad-value who x cnv? orig initial-x)]))
  ;; write-json-array : List -> _
  (define (write-json-array x)
    (write-bytes #"[" out)
    (when (pair? x)
      (write-jsval (car x))
      (for ([xe (in-list (cdr x))])
        (write-bytes #"," out)
        (write-jsval xe)))
    (write-bytes #"]" out))
  ;; write-json-object : Hash Boolean Any -> _
  (define (write-json-object obj cnv? orig)
    (define first? #t)
    (define (write-hash-kv k v)
      (if first? (set! first? #f) (write-bytes #"," out))
      (cond [(symbol? k) (write-json-string (symbol->immutable-string k) out encode)]
            [(and cnv? (string? k)) (write-json-string k out encode)]
            [else (err:bad-key who obj cnv? orig initial-x k)])
      (write-bytes #":" out)
      (write-jsval v))
    (write-bytes #"{" out)
    (hash-for-each obj write-hash-kv #t)
    (write-bytes #"}" out))
  ;; ----
  (unless (memq encode '(all control))
    (error who "bad encode symbol\n  given: ~e" encode))
  (write-jsval initial-x)
  (void))

(define (write-json-string str out encode)
  (define rx-to-encode
    (case encode
      [(control) #rx"[\0-\37\\\"\177]"]
      [(all)     #rx"[\0-\37\\\"\177-\U10FFFF]"]))
  (define (escape m)
    (escape1 (string-ref m 0)))
  (define (escape1 ch)
    (case ch
      [(#\backspace) "\\b"]
      [(#\newline) "\\n"]
      [(#\return) "\\r"]
      [(#\page) "\\f"]
      [(#\tab) "\\t"]
      [(#\\) "\\\\"]
      [(#\") "\\\""]
      [else (let ([n (char->integer ch)])
              (cond [(< n #x10000) (u-esc n)]
                    [else (let ([n (- n #x10000)])
                            (string-append
                             (u-esc (+ #xD800 (arithmetic-shift n -10)))
                             (u-esc (+ #xDC00 (bitwise-and n #x3FF)))))]))]))
  (define (u-esc n)
    (define str (string-upcase (number->string n 16)))
    (define pad (case (string-length str)
                  [(1) "000"] [(2) "00"] [(3) "0"] [else ""]))
    (string-append "\\u" pad str))
  ;; ----
  (write-bytes #"\"" out)
  ;; (write-string (regexp-replace* rx-to-encode str escape) out)
  (let loop ([start 0])
    (cond [(regexp-match-positions rx-to-encode str start)
           => (lambda (m)
                (define mid (caar m))
                (write-string str out start mid)
                (write-string (escape1 (string-ref str mid)) out)
                (loop (add1 mid)))]
          [else (write-string str out start)]))
  (write-bytes #"\"" out))

(define (inexact-rational? x) ; not nan or inf
  (and (inexact-real? x) (rational? x)))

(define (err:bad-value who x cnv? orig initial-x)
  (cond [cnv?
         (error who (string-append
                     "convert function returned bad JSON value"
                     "\n  got: ~e\n  from: ~e\n  within: ~e")
                x orig initial-x)]
        [else
         (error who "bad JSON value\n  got: ~e\n  within: ~e" x initial-x)]))

(define (err:bad-key who x orig-x initial-x k)
  (error who (string-append
              "bad JSON object key"
              "\n  got: ~e\nobject: ~e\n  within: ~e")
         k x initial-x))

(define (jsexpr->bytes+ x
                        #:convert [convert #f]
                        #:encode [encode 'control]
                        #:who [who 'jsexpr->bytes+])
  (define out (open-output-bytes))
  (write-json+ x out #:convert convert #:encode encode #:who who)
  (get-output-bytes out))

(define (jsexpr->string+ x
                         #:convert [convert #f]
                         #:encode [encode 'control]
                         #:who [who 'jsexpr->string+])
  (define out (open-output-string))
  (write-json+ x out #:convert convert #:encode encode #:who who)
  (get-output-string out))

;; ============================================================
;; Parsing (Tokenizer)

(define (read-json-token in)
  (skip-whitespace/peek-char in)
  (read-json-token* in))

(define (read-json-token* in)
  (define ch (peek-char in))
  (case ch
    [(#\") (read-json-string in)]
    [(#\[) (begin (read-byte in) 'start-array)]
    [(#\]) (begin (read-byte in) 'end-array)]
    [(#\{) (begin (read-byte in) 'start-object)]
    [(#\}) (begin (read-byte in) 'end-object)]
    [(#\,) (begin (read-byte in) 'comma)]
    [(#\:) (begin (read-byte in) 'colon)]
    [(#\t #\f #\n) (read-json-literal in)]
    [(#\-) (read-json-number in)]
    [else (cond [(eof-object? ch) (begin (read-byte in) 'EOF)]
                [(and (char<=? #\0 ch #\9)) (read-json-number in)]
                [else (read-json-bad in #f)])]))

;; json-whitespace : Char -> Boolean
(define (json-whitespace? ch)
  (or (eq? ch #\space)
      (eq? ch #\tab)
      (eq? ch #\newline)
      (eq? ch #\return)))

;; skip-whitespace/peek-char : InputPort -> (U Char EOF)
(define (skip-whitespace/peek-char in)
  (let loop ()
    (define ch (peek-char in))
    (cond [(and (char? ch) (json-whitespace? ch))
           (begin (read-char in) (loop))]
          [else ch])))

;; Reference: https://datatracker.ietf.org/doc/html/rfc7159, Section 6
(define json-number-rx
  #px#"^(-?(?:0|[1-9][0-9]*))([.][0-9]+)?(?:[eE]([-+]?[0-9]+))?")
(define json-literal-rx
  #px#"^(true|false|null)(?=$|[^a-zA-Z0-9_])")
(define json-string-rx
  #px#"^\"((?:[^\"\\\\]|\\\\[bnrft\"\\\\/]|\\\\u[0-9a-fA-F]{4})*)\"")

(define (read-json-number in)
  (cond [(regexp-try-match json-number-rx in)
         => (lambda (m) (cons 'number m))]
        [else (read-json-bad in 'number)]))

(define (read-json-literal in)
  (cond [(regexp-try-match json-literal-rx in)
         => (lambda (m)
              (define word (cadr m))
              (cond [(equal? word "true") 'true]
                    [(equal? word "false") 'false]
                    [(equal? word "null") 'null]))]
        [else (read-json-bad in 'literal)]))

(define (read-json-string in)
  (cond [(regexp-try-match json-string-rx in)
         => (lambda (m) (cons 'string (cdr m)))]
        [else (read-json-bad in 'string)]))

(define (read-json-bad in tried)
  (list 'error tried))

(define (parse-json-number whole-bs frac-bs ex-bs)
  (define whole (string->number (bytes->string/latin-1 whole-bs)))
  (define frac (and frac-bs (string->number (bytes->string/latin-1 frac-bs))))
  (define ex (and ex-bs (string->number (bytes->string/latin-1 ex-bs))))
  (define n (+ whole (or frac 0)))
  (cond [ex
         (define oom (if (zero? n) exp (+ ex (log (abs n) 10))))
         (cond [(< oom -400)
                (if (negative? n) -0.0 0.0)]
               [(> oom 400)
                (cond [(zero? n) 0.0] [(negative? n) -inf.0] [else +inf.0])]
               [else (exact->inexact (* n (expt 10 ex)))])]
        [else n]))

(define (parse-json-string bs)
  (define who 'parse-json-string)
  (define (find-next-backslash start)
    (for/first ([b (in-bytes bs start)]
                [i (in-naturals start)]
                #:when (= b (char->integer #\\)))
      i))
  (define (content-loop start acc)
    (content-loop* start (find-next-backslash start) acc))
  (define (content-loop* start idx acc)
    (cond [idx
           (define s1 (bytes->string/utf-8 bs start idx))
           (escape-loop idx (cons s1 acc))]
          [else
           (define s1 (bytes->string/utf-8 bs start))
           (finish (cons s1 acc))]))
  (define (escape-loop idx acc)
    (define (continue s) (content-loop (+ idx 2) (cons s acc)))
    (define ch (integer->char (bytes-ref bs (+ idx 1))))
    (case ch
      [(#\b) (continue "\b")]
      [(#\n) (continue "\n")]
      [(#\r) (continue "\r")]
      [(#\f) (continue "\f")]
      [(#\t) (continue "\t")]
      [(#\\) (continue "\\")]
      [(#\") (continue "\"")]
      [(#\/) (continue "/")]
      [(#\u)
       (define code-s (bytes->string/latin-1 bs (+ idx 2) (+ idx 6)))
       (define code (number->string code-s 16))
       (cond [(codepoint-hi-surrogate code) ;; hi comes first
              => (lambda (hi)
                   (cond [(and (<= (+ idx 12) (bytes-length bs))
                               (= (bytes-ref bs (+ idx 6)) (char->integer #\\))
                               (= (bytes-ref bs (+ idx 7)) (char->integer #\u)))
                          (define next-s (bytes->string/latin-1 bs (+ idx 8) (+ idx 12)))
                          (define next (number->string next-s 16))
                          (cond [(codepoint-lo-surrogate next)
                                 => (lambda (lo)
                                      (define s1 (string (integer->char (+ hi lo))))
                                      (content-loop (+ idx 12) (cons s1 acc)))]
                                [else (err:incomplete-surrogate who bs idx)])]
                         [else (err:incomplete-surrogate who bs idx)]))]
             [(codepoint-lo-surrogate code)
              (err:unmatched-lo-surrogate who bs idx)]
             [else ;; valid codepoint
              (content-loop (+ idx 6) (cons (string (integer->char code)) acc))])]))
  (define (finish acc)
    (cond [(and (pair? acc) (null? (cdr acc))) (car acc)]
          [else (apply string-append (reverse acc))]))
  ;; ----
  (cond [(find-next-backslash 0)
         => (lambda (idx) (content-loop* 0 idx null))]
        [else (bytes->string/utf-8 bs)]))

(define (err:unmatched-lo-surrogate who bs idx)
  (error who "unmatched low surrogate in JSON string\n  in: ~e"
         (subbytes bs idx (min (+ idx 12) (bytes-length bs)))))

(define (err:incomplete-surrogate who bs idx)
  (error who "incomplete surrogate pair in JSON string\n  in: ~e"
         (subbytes bs idx (min (+ idx 12) (bytes-length bs)))))

(define (codepoint-hi-surrogate code)
  (and (<= #xD800 code #xDBFF)
       (arithmetic-shift (- code #xD800) 10)))

(define (codepoint-lo-surrogate code)
  (and (<= #xDC00 code #xDFFF)
       (- code #xDC00)))

;; ============================================================
;; Parsing

(define (read-json+ in
                    #:parse-number [user-parse-number #f]
                    #:parse-string [user-parse-string #f]
                    #:parse-object [user-parse-object #f]
                    #:parse-array [user-parse-array #f]
                    #:convert [on-value #f]
                    #:allow-eof? [allow-eof? #t]
                    #:srcloc? [srcloc? #f]
                    #:who [who 'read-json+])
  (define (parse-number loc num-bs whole-bs frac-bs ex-bs)
    (if user-parse-number
        (user-parse-number loc num-bs whole-bs frac-bs ex-bs)
        (parse-json-number whole-bs frac-bs ex-bs)))
  (define (parse-string loc str-bs)
    (if user-parse-string
        (user-parse-string loc str-bs)
        (parse-json-string str-bs)))

  (define user-array-init (and user-parse-array (car user-parse-array)))
  (define user-array-update (and user-parse-array (caddr user-parse-array)))
  (define user-array-finish (and user-parse-array (caddr user-parse-array)))

  (define user-object-init (and user-parse-object (car user-parse-object)))
  (define user-object-update (and user-parse-object (caddr user-parse-object)))
  (define user-object-finish (and user-parse-object (caddr user-parse-object)))

  (define (array-init) (if user-array-init (user-array-init) null))
  (define (array-finish vs) (if user-array-finish (user-array-finish vs) (reverse vs)))
  (define (array-update v vs) (if user-array-update (user-array-update v vs) (cons v vs)))

  (define (object-init) (if user-object-init (user-object-init) (hasheq)))
  (define (object-finish obj) (if user-object-finish (user-object-finish obj) obj))
  (define (object-update obj k v)
    (if user-object-update
        (user-object-update obj k v)
        (let ([k (string->symbol k)])
          (if (hash-has-key? obj k) obj (hash-set obj k v)))))

  (define (read-value allow-eof? expected)
    (define-values (next loc) (skip/read-json-token in srcloc?))
    (read-value* allow-eof? expected next loc))

  (define (read-value* allow-eof? expected next loc)
    (define (finish v [loc loc])
      (if on-value (on-value loc v) v))
    (match next
      [(list 'number num-bs whole-bs frac-bs ex-bs)
       (finish (parse-number loc num-bs whole-bs frac-bs ex-bs))]
      [(list 'string str-bs)
       (finish (parse-string loc str-bs))]
      ['true (finish #t)]
      ['false (finish #f)]
      ['null (finish 'null)]
      ['start-array
       (define v (read-array))
       (finish v (update-loc loc in))]
      ['start-object
       (define v (read-object))
       (finish v (update-loc loc in))]
      ['EOF #:when allow-eof? eof]
      [_ (let ([expected0 '(ATOM start-array start-object)])
           (err:bad-token who next loc (append expected expected0)))]))

  (define (read-array)
    (define-values (next loc) (skip/read-json-token in srcloc?))
    (define acc (array-init))
    (match next
      ['end-array (array-finish acc)]
      [_ (let ([v (read-value* #f '(end-array) next loc)])
           (read-array-rest (array-update v acc)))]))

  (define (read-array-rest acc)
    (define-values (next loc) (skip/read-json-token in srcloc?))
    (match next
      ['end-array (array-finish acc)]
      ['comma (read-array-rest (array-update (read-value #f '()) acc))]
      [_ (err:bad-token who next loc '(end-array comma))]))

  (define (read-object)
    (define-values (next loc) (skip/read-json-token in srcloc?))
    (define acc (hasheq))
    (match next
      ['end-object (object-finish acc)]
      [_ (let-values ([(k v) (read-entry* next loc '(end-object))])
           (read-object-rest (object-update acc k v)))]))

  (define (read-object-rest acc)
    (define-values (next loc) (skip/read-json-token in srcloc?))
    (match next
      ['end-object (object-finish acc)]
      ['comma (let-values ([(k v) (read-entry)])
                (read-object-rest (object-update acc k v)))]
      [_ (err:bad-token who next loc '(end-object comma))]))

  (define (read-entry)
    (define-values (next loc) (skip/read-json-token in srcloc?))
    (read-entry* next loc '()))
  (define (read-entry* next loc expected)
    (match next
      [(list 'string str-bs)
       (define k (parse-string loc str-bs))
       (define-values (next2 loc2) (skip/read-json-token in srcloc?))
       (match next2
         ['colon
          (define v (read-value #f null))
          (values k v)]
         [_ (err:bad-token who next loc '(colon))])]
      [_ (err:bad-token who next loc (append '(string) expected))]))

  ;; ----
  (read-value allow-eof? null))

(define (skip/read-json-token in srcloc?)
  (skip-whitespace/peek-char in)
  (define getloc (and srcloc? (get-srcloc in)))
  (define next (read-json-token in))
  (values next (and getloc (getloc))))

(define (get-srcloc in)
  (define source (object-name in))
  (define-values (line col pos) (port-next-location in))
  (lambda ()
    (define-values (line2 col2 pos2) (port-next-location in))
    (srcloc source line col pos (- pos2 pos))))

(define (update-loc loc in)
  (define-values (line2 col2 pos2) (port-next-location in))
  (match loc
    [(srcloc source line col pos span)
     (srcloc source line col pos (- pos2 pos))]
    [#f #f]))

(define (err:bad-token who next loc expected)
  ;; FIXME: EOF, error token
  (error who "unexpected token\n  got: ~a\n  expected: ~a"
         (token->token-description next)
         (string-join (map token->token-description expected) " or ")))

(define (token->token-description token)
  (match token
    ['start-array "'[' (start array)"]
    ['end-array "']' (end array)"]
    ['start-object "'{' (start object)"]
    ['end-object "'}' (end object)"]
    ['comma "',' (field separator)"]
    ['colon "':' (key-value separator)"]
    ['ATOM "atomic value (number, string, or literal)"]
    [(cons (? symbol? s) _) (symbol->string s)]
    [(? symbol? s) (symbol->string s)]))

#;
(define (string->jsexpr+ s
                         #:convert [on-value #f]
                         #:srcloc? [srcloc? #f])
  (define who 'string->jsexpr+)
  (define READ-LEN 40)
  (define in (open-input-string s))
  (when srcloc? (port-count-lines! in))
  (define v (read-json+ in #:allow-eof? #f #:convert on-value #:srcloc? srcloc? #:who who))
  (unless (eof-object? (skip-whitespace/peek-char in))
    (error 'string->jsexpr "content after JSON value\n  content: ~e"
           (read-string in READ-LEN)))
  v)
