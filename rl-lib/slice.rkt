#lang racket/base
(require racket/contract
         racket/match
         racket/struct
         racket/pretty)
(provide bytes-slice?
         string-slice?
         vector-slice?
         (contract-out
          #:unprotected-submodule unchecked
          [print-slice-contructor (parameter/c (listof (or/c #t #f 0 1)))]
          [struct slice ([value (or/c bytes? string? vector? slice?)]
                         [start exact-nonnegative-integer?]
                         [end exact-nonnegative-integer?])]
          [slice-length
           (-> slice? any)]
          [slice-contents
           (-> slice? any)]
          [bytes-slice->string/utf-8
           (->* [bytes-slice?] [(or/c char? #f)] string?)]
          [string-slice->bytes/utf-8
           (-> string-slice? bytes?)]))

(define print-slice-constructor (make-parameter '(0 1)))

(struct slice (value start end)
  #:guard (lambda (value start end _name)
            (define len
              (cond [(bytes? value) (bytes-length value)]
                    [(string? value) (string-length value)]
                    [(vector? value) (vector-length value)]
                    [(slice? value) (slice-length value)]))
            (unless (<= start end len)
              (define what
                (cond [(bytes? value) "bytes"]
                      [(string? value) "string"]
                      [(vector? value) "vector"]
                      [(slice? value) "slice"]))
              (raise-range-error 'slice what "ending " end value start length 0))
            (let ([value (if (slice? value) (slice-value value) value)]
                  [offset (if (slice? value) (slice-start value) 0)])
              (cond [(= start end)
                     (values (cond [(bytes? value) #""]
                                   [(string? value) ""]
                                   [(vector? value) #()])
                             0 0)]
                    [else (values value (+ offset start) (+ offset end))])))
  #:property prop:custom-write
  (let ([writer (make-constructor-style-printer
                 (lambda (self) 'slice)
                 (match-lambda [(slice value start end) (list value start end)]))])
    (lambda (self out mode)
      (if (memv mode (print-slice-contructor-modes))
          (writer self out mode)
          (case mode
            [(#f) (display (slice-contents self) out)]
            [(#t) (write (slice-contents self) out)]
            [else (print (slice-contents self) out mode)]))))
  #:property prop:custom-print-quotable 'self)

(define (slice-length s)
  (match s [(slice _ start end) (- end start)]))

(define (slice-contents s)
  (match s
    [(slice v start end)
     (cond [(= start end) v] ;; relies on slice guard invariant
           [(bytes? v) (subbytes v start end)]
           [(string? v) (substring v start end)]
           [(vector? v)
            (define vec (make-vector (- end start)))
            (vector-copy! vec 0 v start end)
            vec])]))

(define (string-slice? v)
  (and (slice? v) (string? (slice-value v))))
(define (bytes-slice? v)
  (and (slice? v) (bytes? (slice-value v))))
(define (vector-slice? v)
  (and (slice? v) (vector? (slice-value v))))

(define (bytes-slice->string/utf-8 s [err-char #f])
  (match s
    [(slice bs start end)
     (bytes->string/utf-8 bs err-char start end)]))

(define (string-slice->bytes/utf-8 s)
  (match s
    [(slice str start end)
     (string->bytes/utf-8 s #f start end)]))
