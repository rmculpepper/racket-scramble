;; Copyright 2023-2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/contract
         racket/match
         "immutable.rkt")
(provide vectorof/ic
         hash/ic
         hash-record/ic
         make-string/ic
         make-bytes/ic
         string/ic
         bytes/ic
         convert/ic)

;; Like vectorof, hash/c, etc, but
;; - always produce an immutable value
;; - always produce authentic value (not chaperone/impersonator)
;; - if contents all eq?, and already immutable+authentic, return existing value

;; Note: all of these are impersonator contracts, because chaperone-of? does not
;; accept a change from mutable to immutable (or a copy of a mutable value).

(define (vectorof/ic elem/c)
  (define who 'vectorof/ic)
  (define elem-ctc (coerce-contract who elem/c))
  (define name (list who (contract-name elem-ctc)))
  (define get-elem-proj (contract-late-neg-projection elem-ctc))
  (define (vectorof/ic-late-neg-projection blame)
    (define elem-proj (get-elem-proj (blame-add-context blame "an element of")))
    (lambda (v missing-party)
      (cond [(vector? v)
             (define v2
               (cond [(immutable-authentic? v) #f]
                     [else (make-vector (vector-length v))]))
             (for ([i (in-naturals)] [elem (in-vector v)])
               ;; INV: v2 is vector iff cannot reuse v, else v2 is #f
               (define elem2 (elem-proj elem missing-party))
               (when (not (eq? elem elem2)) ;; cannot reuse v
                 (when (not v2)
                   (set! v2 (make-vector (vector-length v)))
                   (vector-copy! v2 0 v 0 i)))
               (when v2 (vector-set! v2 i elem2)))
             (if v2 (vector->immutable-vector v2) v)]
            [else (raise-blame-error blame v #:missing-party missing-party
                                     '(expected: "vector" given: "~e") v)])))
  (make-contract
   #:name name
   #:first-order vector?
   #:late-neg-projection vectorof/ic-late-neg-projection))

(define (hash/ic key/c val/c)
  (define key-ctc (coerce-contract 'hash/ic key/c))
  (define val-ctc (coerce-contract 'hash/ic val/c))
  (define name (list 'hash/ic (contract-name key-ctc) (contract-name val-ctc)))
  (define get-key-proj (contract-late-neg-projection key-ctc))
  (define get-val-proj (contract-late-neg-projection val-ctc))
  (define (hash/ic-late-neg-projection blame)
    (define key-proj (get-key-proj (blame-add-context blame "a key of")))
    (define val-proj (get-val-proj (blame-add-context blame "a value of")))
    (lambda (val missing-party)
      (if (hash? val)
          (for/fold ([h (hash-copy-clear val #:kind 'immutable)]
                     [reuse-val? (immutable-authentic? val)]
                     #:result (if reuse-val? val h))
                    ([(k v) (in-hash val)])
            (define k2 (key-proj k missing-party))
            (define v2 (val-proj v missing-party))
            (values (hash-set h k2 v2)
                    (and reuse-val? (eq? k k2) (eq? v v2))))
          (raise-blame-error blame val #:missing-party missing-party
                             '(expected: "hash" given: "~e") val))))
  (make-contract
   #:name name
   #:first-order hash?
   #:late-neg-projection hash/ic-late-neg-projection))

(define (hash-record/ic key=>value/c
                        #:name [name '(hash-record/ic ....)]
                        #:other [get-other/c #f]
                        #:required [required 'all])
  (define who 'hash-record/ic)
  (define key=>ctx+proj
    (for/fold ([h (hash-copy-clear key=>value/c #:kind 'immutable)])
              ([(key value/c) (in-hash key=>value/c)])
      (define key-ctx (string->immutable-string (format "the ~e field of" key)))
      (define proj (get/build-late-neg-projection (coerce-contract who value/c)))
      (hash-set h key (cons key-ctx proj))))
  (unless (or (eq? get-other/c #f)
              (and (procedure? get-other/c) (procedure-arity-includes? get-other/c 1)))
    (raise-argument-error who "(or/c #f (-> any contract?))" get-other/c))
  (unless (or (eq? required 'all) (list? required))
    (raise-argument-error who "(or/c 'all list?)" required))
  (when (list? required)
    (for ([required-key (in-list required)])
      (unless (hash-has-key? key=>ctx+proj required-key)
        (raise-arguments-error who "required key is not a member of keys"
                               "required key" required-key
                               "keys" (hash-keys key=>ctx+proj)))))
  (define required-keys (if (list? required) required (hash-keys key=>value/c)))
  (define (hash-record/ic-late-neg-projection blame)
    (define key=>proj
      (for/fold ([h (hash-copy-clear key=>ctx+proj #:kind 'immutable)])
                ([(key ctx+proj) (in-hash key=>ctx+proj)])
        (match-define (cons ctx proj) ctx+proj)
        (hash-set h key (proj (blame-add-context blame ctx)))))
    (lambda (val missing-party)
      (cond [(hash? val)
             (define h
               (cond [(and (immutable? val) (not (impersonator? val))) val]
                     [else (hash-map/copy val values #:kind 'immutable)]))
             (define checked-h
               (for/fold ([h h]) ([(key keyval) (in-hash h)])
                 (cond [(hash-ref key=>proj key #f)
                        => (lambda (proj)
                             (define keyval* (proj keyval missing-party))
                             (if (eq? keyval* keyval) h (hash-set h key keyval*)))]
                       [(and get-other/c (get-other/c key))
                        => (lambda (keyval/c)
                             (cond [(eq? keyval/c any/c) h]
                                   [else
                                    (define keyval-ctc (coerce-contract who keyval/c))
                                    (define proj (get/build-late-neg-projection keyval-ctc))
                                    (define key-ctx (format "the ~e field of" key))
                                    (define blame* (blame-add-context blame key-ctx))
                                    (define keyval* ((proj blame*) keyval missing-party))
                                    (if (eq? keyval* keyval) h (hash-set h key keyval*))]))]
                       [else h])))
             (for ([key (in-list required-keys)])
               (unless (hash-has-key? checked-h key)
                 (raise-blame-error blame val #:missing-party missing-party
                                    '(expected: "hash with ~e key" given: "~e")
                                    key checked-h)))
             checked-h]
            [else
             (raise-blame-error blame val #:missing-party missing-party
                                '(expected: "hash" given: "~e") val)])))
  (make-contract
   #:name name
   #:first-order hash?
   #:late-neg-projection hash-record/ic-late-neg-projection))

(define (make-bytes/ic pred/rx #:name [name0 #f])
  (define name
    (cond [name0 name0]
                [(or (regexp? pred/rx) (byte-regexp? pred/rx))
                 `(make-bytes/ic ,pred/rx)]
                [else '(make-bytes/ic ...)]))
  (make-contract
   #:name name
   #:first-order bytes?
   #:late-neg-projection
   (lambda (blame)
     (lambda (v missing-party)
       (define (bad v fmt . fmt-vs)
         (apply raise-blame-error blame v #:missing-party missing-party
                (append fmt '(given: "~e")) (append fmt-vs (list v))))
       (unless (bytes? v) (bad v '(expected: "bytes")))
       (define v* (bytes->immutable-bytes v))
       (when (or (regexp? pred/rx) (byte-regexp? pred/rx))
         (unless (regexp-match? pred/rx v*)
           (bad v* '(expected: "bytes matching ~e") pred/rx)))
       (when (procedure? pred/rx)
         (unless (pred/rx v*)
           (bad v* '(expected: "bytes satisfying ~e") pred/rx)))
       v*))))

(define bytes/ic (make-bytes/ic #f #:name 'bytes/ic))

(define (make-string/ic pred/rx #:name [name0 #f])
  (define name
    (cond [name0 name0]
          [(or (regexp? pred/rx) (byte-regexp? pred/rx))
           `(make-string/ic ,pred/rx)]
          [else '(make-string/ic ...)]))
  (make-contract
   #:name name
   #:first-order string?
   #:late-neg-projection
   (lambda (blame)
     (lambda (v missing-party)
       (define (bad v fmt . fmt-vs)
         (apply raise-blame-error blame v #:missing-party missing-party
                (append fmt '(given: "~e")) (append fmt-vs (list v))))
       (unless (string? v) (bad v '(expected: "string")))
       (define v* (string->immutable-string v))
       (when (or (regexp? pred/rx) (byte-regexp? pred/rx))
         (unless (regexp-match? pred/rx v*)
           (bad v* '(expected: "string matching ~e") pred/rx)))
       (when (procedure? pred/rx)
         (unless (pred/rx v*)
           (bad v* '(expected: "string satisfying ~e") pred/rx)))
       v*))))

(define string/ic (make-string/ic #f #:name 'string/ic))

;; convert/ic : (X -> Y) -> Contract
;; PRE: convert should be idempotent
(define (convert/ic convert
                    #:name [name '(convert/ic ....)]
                    #:exn->lines [exn->lines default-exn->lines])
  (define (raised->lines e)
    (cond [(exn? e) (exn->lines e)]
          [else (format "\n  raised by conversion: ~e" e)]))
  (make-contract
   #:name name
   #:late-neg-projection
   (lambda (blame)
     (lambda (val missing-party)
       (with-handlers ([(lambda (e) #t)
                        (lambda (e)
                          (raise-blame-error
                           blame #:missing-party missing-party
                           val
                           '(expected: "~.s" given: "~e" "~a")
                           name val (raised->lines e)))])
         (convert val))))))

(define (default-exn->lines e)
  (format "\n  error: ~.a" (regexp-replace #rx"\n.*$" (exn-message e) "...")))
