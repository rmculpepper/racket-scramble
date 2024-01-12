#lang racket/base
(require racket/contract
         rackunit
         scramble/contract
         scramble/immutable)

(test-case "vectorof/ic"
  (define c (vectorof/ic real?))
  (define/contract f (-> c any) values)
  (define/contract g (-> any/c c) values)

  (define v1 (vector 1 2 3))
  (define v1c (f v1))
  (check-not-eq? v1 v1c)
  (begin (vector-set! v1 0 'a)
         (check-equal? v1c (vector 1 2 3)))

  (check-exn #rx"f: contract violation\n  expected: vector\n  given: 'a"
             (lambda () (f 'a)))
  (check-exn #rx"f: contract violation\n  expected: real[?]\n"
             (lambda () (f (vector 'a))))
  (check-exn #rx"g: broke its own contract\n  promised: vector\n  produced: 'a"
             (lambda () (g 'a))))

(test-case "hash/ic"
  (define c (hash/ic symbol? string?))
  
  (define/contract f (-> c any) values)
  (define/contract g (-> any/c c) values)

  (define h1 (hash 'a "A" 'b "BB"))
  (define h1c (f h1))
  (check-eq? h1 h1c)
  (check-pred immutable-authentic? h1c)

  (define h2 (make-hasheq))
  (hash-set! h2 'a "A")
  (hash-set! h2 'b "BB")
  (define h2c (f h2))
  (check-not-eq? h2 h2c)
  (check-pred immutable-authentic? h2c)
  (check-equal? h2c (hasheq 'a "A" 'b "BB"))

  (check-exn #rx"f: contract violation\n  expected: hash\n"
             (lambda () (f 'a)))
  (check-exn #rx"f: contract violation\n  expected: symbol[?]\n"
             (lambda () (f (hash "a" "A"))))
  (check-exn #rx"f: contract violation\n  expected: string[?]\n"
             (lambda () (f (hasheq 'a 123)))) )

(test-case "make-bytes/ic"
  (define c (make-bytes/ic #rx"AAA"))

  (define/contract f (-> c any) values)
  (define/contract g (-> any/c c) values)

  (define bs1 (bytes-copy #"BAAAC"))
  (define bs1c (f bs1))
  (check-not-eq? bs1 bs1c)
  (bytes-set! bs1 2 0)
  (check-equal? bs1c #"BAAAC")

  (check-exn #rx"f: contract violation\n  expected: bytes\n"
             (lambda () (f 'a)))
  (check-exn #rx"f: contract violation\n  expected: bytes matching"
             (lambda () (f #"ABC"))))

(test-case "make-string/ic"
  (define c (make-string/ic #rx"AAA"))

  (define/contract f (-> c any) values)
  (define/contract g (-> any/c c) values)

  (define s1 (string-copy "BAAAC"))
  (define s1c (f s1))
  (check-not-eq? s1 s1c)
  (string-set! s1 2 #\X)
  (check-equal? s1c "BAAAC")

  (check-exn #rx"f: contract violation\n  expected: string\n"
             (lambda () (f 'a)))
  (check-exn #rx"f: contract violation\n  expected: string matching"
             (lambda () (f "ABC"))))

(test-case "convert/ic"
  (define (to-symbol v)
    (cond [(string? v) (string->symbol v)]
          [(symbol? v) v]
          [else (error 'to-symbol "cannot convert to symbol\n  value: ~e" v)]))
  (define c (convert/ic to-symbol #:name 'symbolable))

  (define/contract f (-> c any) values)
  (define/contract g (-> any/c c) values)

  (check-equal? (f 'x) 'x)
  (check-equal? (f "x") 'x)
  (check-exn #rx"f: contract violation\n  expected: symbolable\n  given: 123"
             (lambda () (f 123))))
