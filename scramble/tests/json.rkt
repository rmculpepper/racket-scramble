#lang racket/base
(require (for-syntax racket/base)
         racket/match
         syntax/location
         rackunit
         scramble/private/json)

(begin-for-syntax
  (define ((tc-macro fun-id) stx)
    (syntax-case stx ()
      [(_ arg ...)
       #`(#,fun-id (quote-srcloc #,stx) arg ...)])))
(define-syntax tw (tc-macro #'tw*))
(define-syntax twerr (tc-macro #'twerr*))
(define-syntax twc (tc-macro #'twc*))
(define-syntax twcerr (tc-macro #'twcerr*))
(define-syntax tr (tc-macro #'tr*))

(define (tw* src v str)
  (test-case (srcloc->string src)
    (check-equal? (jsexpr->string+ v #:encode 'all) str)))

(define (twerr* src v #:err [pred/rx exn:fail?])
  (test-case (srcloc->string src)
    (check-exn pred/rx (lambda () (jsexpr->string+ v #:encode 'all)))))

(define (twc* src v str)
  (test-case (srcloc->string src)
    (check-equal? (jsexpr->string+ v #:convert the-convert #:encode 'all) str)))

(define (twcerr* src v #:err [pred/rx exn:fail?])
  (test-case (srcloc->string src)
    (check-exn pred/rx (lambda () (jsexpr->string+ v #:convert the-convert #:encode 'all)))))

(define (tr* src str v)
  (test-case (srcloc->string src)
    (define in (open-input-string str))
    (port-count-lines! in)
    (check-equal? (read-json+ in #:convert the-convert-in) v)))

(define (trerr* src str #:err [pred/rx exn:fail?])
  (test-case (srcloc->string src)
    (define in (open-input-string str))
    (port-count-lines! in)
    (check-exn pred/rx (lambda () (read-json+ in #:convert the-convert-in)))
    (void)))

;; ----------------------------------------

(struct point (x y) #:transparent)

(define (the-convert v)
  (match v
    [(point x y) (hasheq 'x x 'y y)]
    [(? keyword?) (string->symbol (keyword->string v))] ;; note: bad
    ['+inf.0 (unquoted-printing-string "1e400")]
    [_ v]))

(define (the-convert-in loc v)
  (match v
    [(hash-table ['x x] ['y y]) (point x y)]
    [_ v]))

(tw 0 "0")
(tw 1234 "1234")
(tw 12.34 "12.34")
(tw 'null "null")
(tw #t "true")
(tw #f "false")
(tw "abc" "\"abc\"")
(tw (string (integer->char 225) (integer->char 119070)) "\"\\u00E1\\uD834\\uDD1E\"")
(tw '() "[]")
(tw '(1 2 3) "[1,2,3]")
(tw (hasheq) "{}")
(tw (hasheq 'a 1 'b 2) "{\"a\":1,\"b\":2}")

(twerr +inf.0 #:err #rx"bad JSON value")
(twerr (point 1 2) #:err #rx"bad JSON value")
(twerr (list 1 'a) #:err #rx"bad JSON value")

(twc +inf.0 "1e400")
(twc (list (point 1 2) (point 3 4))
     "[{\"x\":1,\"y\":2},{\"x\":3,\"y\":4}]")
(twcerr (list 1 '#:a) #:err #rx"convert function returned bad JSON value")

(tr (jsexpr->string+ (list "hello" (hash 'x 1 'y 2)))
    (list "hello" (point 1 2)))
(tr "\"\u00E1\uD834\uDD1E\"" (string (integer->char #xE1) (integer->char #x1D11E)))
