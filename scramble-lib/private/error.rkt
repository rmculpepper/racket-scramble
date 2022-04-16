;; Copyright 2020 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(provide raise-kw-argument-error)

;; Contract errors in raise-kw-argument-error are reported in terms of
;; raise-kw-argument-error* because ...

(define raise-kw-argument-error
  (make-keyword-procedure
   (lambda (kws kwargs who expected index . pos-args)
     (raise-kw-argument-error* who expected index pos-args kws kwargs))))

(define (raise-kw-argument-error* who wanted index [pos-args null] [kws null] [kwargs null])
  (define myself 'raise-keyword-argument-error*)
  (let ()
    (define (bad n expected)
      (raise-argument-error myself expected n who wanted index pos-args kws kwargs))
    (unless (symbol? who) (bad 0 "symbol?"))
    (unless (string? wanted) (bad 1 "string?"))
    (unless (or (exact-nonnegative-integer? index) (keyword? index))
      (bad 2 "(or/c exact-nonnegative-integer? keyword?)"))
    (unless (list? pos-args) (bad 3 "list?"))
    (unless (and (list? kws) (andmap keyword? kws)) (bad 4 "(listof keyword?)"))
    (unless (list? kwargs) (bad 5 "list?"))
    (unless (= (length kws) (length kwargs))
      (raise-arguments-error myself
                             "keywords and keyword arguments have different lengths"
                             "keywords" kws "keyword arguments" kwargs))
    (cond [(exact-nonnegative-integer? index)
           (unless (< index (length pos-args))
             (raise-range-error myself "list" "" index pos-args 0 (sub1 (length pos-args)) #f))]
          [else
           (unless (memq index kws)
             (raise-arguments-error myself "index is not in given keywords"
                                    "index" index "keywords" kws))]))
  (define (find-given)
    (if (keyword? index)
        (for/first ([kw (in-list kws)] [kwarg (in-list kwargs)] #:when (eq? kw index)) kwarg)
        (list-ref pos-args index)))
  (define (show-other-args)
    (apply string-append
           (append
            (for/list ([other-arg (in-list pos-args)]
                       [other-index (in-naturals)]
                       #:when (not (eqv? other-index index)))
              (format "\n   ~e" other-arg))
            (for/list ([other-kw (in-list kws)]
                       [other-kwarg (in-list kwargs)]
                       #:when (not (eqv? other-kw index)))
              (format "\n   ~.a ~e" other-kw other-kwarg)))))
  (define (nth n0)
    (define n (add1 n0))
    (cond [(<= 4 n 20) (format "~ath" n)]
          [else (case (remainder n 10)
                  [(1) (format "~ast" n)]
                  [(2) (format "~and" n)]
                  [(3) (format "~ard" n)]
                  [else (format "~ath" n)])]))
  (define msg
    (format (string-append
             "~.a: contract violation"
             "\n  expected: ~a"
             "\n  given: ~e"
             "\n  argument position: ~a"
             "\n  other arguments...:~a")
            who wanted (find-given)
            (if (keyword? index) (format "the ~a keyword argument" index) (nth index))
            (show-other-args)))
  (let/ec k
    (raise (exn:fail:contract msg (continuation-marks k)))))
