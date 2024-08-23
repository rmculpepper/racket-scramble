;; Copyright 2024 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/contract/base
         racket/match
         racket/sequence
         racket/list
         "contract.rkt")
(provide relation?
         relation-heading
         relation-tuples
         (rename-out [relation* relation])
         (contract-out
          #:unprotected-submodule unchecked
          [make-relation
           (-> (or/c #f symbol?) (vectorof/ic symbol?) (vectorof/ic vector?)
               relation?)]
          [relation-add-lookup
           (-> relation? symbol? boolean?
               relation?)]
          [relation-index
           (-> relation? symbol?
               (or/c #f exact-nonnegative-integer?))]
          [in-relation
           (-> relation? (or/c symbol? (listof symbol?))
               sequence?)]
          [relation-find
           (->* [relation? symbol? any/c] [boolean?]
                any)]
          [relation-ref
           (->* [relation? symbol? any/c (or/c symbol? (listof symbol?))] [any/c]
                any)]
          [relation-ref-all
           (-> relation? symbol? any/c (or/c symbol? (listof symbol?))
               any)]
          [relation-prepare-find
           (->* [relation? symbol?] [boolean?]
                procedure?)]
          [relation-prepare-ref
           (-> relation? symbol? (or/c symbol? (listof symbol?))
               procedure?)]
          [relation-prepare-ref-all
           (-> relation? symbol? (or/c symbol? (listof symbol?))
               procedure?)]
          ))

;; ============================================================
;; Relations

;; "index" = position of a field in a vector
;; "lookup" = fast retrieval structure (usually called "index" in db contexts)

;; A Relation is (relation Any Vector[Symbol] Vector[Vector] Hasheqv[Nat => Lookup])
;; where Lookup is (cons unique? Hash).
(struct relation (name heading tuples lookups)
  #:property prop:custom-write
  (lambda (self out mode)
    (cond [(relation-name self)
           => (lambda (name)
                (fprintf out "#<relation:~.a>" name))]
          [else (write-string "#<relation>" out)])))

(define-syntax (relation* stx)
  (define-splicing-syntax-class name-decl
    (pattern (~seq #:name name:expr))
    (pattern (~seq) #:with name #'(quote #f)))
  (define-splicing-syntax-class heading-decl
    (pattern (~seq #:heading [field:expr ...])))
  (define-splicing-syntax-class tuples-decl
    (pattern (~seq #:tuples [value:expr ...] ...)))
  (define-splicing-syntax-class lookup-decl
    (pattern (~seq #:unique field:expr) #:with unique? #'(quote #t))
    (pattern (~seq #:lookup field:expr) #:with unique? #'(quote #f)))
  (syntax-parse stx
    [(_ n:name-decl h:heading-decl t:tuples-decl i:lookup-decl ...)
     #'(build-relation n.name
                       (vector-immutable h.field ...)
                       (vector-immutable (vector-immutable t.value ...) ...)
                       (list i.field ...)
                       (list i.unique? ...))]))

(define (build-relation name heading tuples lu-fields lu-types)
  (for/fold ([rel (make-relation name heading tuples)])
            ([lu-field (in-list lu-fields)]
             [lu-unique? (in-list lu-types)])
    (relation-add-lookup rel lu-field lu-unique? #:who 'relation)))

(define (make-relation name heading tuples)
  (define nfields (vector-length heading))
  ;; check heading
  (for ([f (in-vector heading)])
    (unless (symbol? f) (raise-argument-error 'relation "symbol?" f)))
  (let ([dup (check-duplicates (vector->list heading))])
    (when dup (error 'relation "duplicate field name" dup)))
  ;; check tuples
  (for ([tuple (in-vector tuples)])
    (unless (= (vector-length tuple) nfields)
      (error 'relation
             "wrong number of fields in tuple~a\n  expected: ~s fields\n  tuple: ~e"
             (if name (format "\n  relation name: ~e" name) "")
             nfields tuple)))
  (relation name heading tuples (hasheqv)))

;; relation-add-lookup : Relation Symbol Boolean -> Relation
(define (relation-add-lookup rel keyfield unique? #:who [who 'relation-add-lookup])
  (match-define (relation name heading tuples lookups) rel)
  (define keyindex (get-index who rel keyfield))
  (match (hash-ref lookups keyindex #f)
    [(cons #t lu)
     rel]
    [(cons #f lu)
     #:when (not unique?)
     rel]
    [_
     (define lookup
       (cond [unique?
              (for/fold ([h (hash)]) ([tuple (in-vector tuples)])
                (define keyvalue (vector-ref tuple keyindex))
                (when (hash-has-key? h keyvalue)
                  (error who "~a\n  relation: ~e\n  field: ~e\n  field value: ~e"
                         "unique constraint violated" rel keyfield keyvalue))
                (hash-set h keyvalue tuple))]
             [else
              (for/fold ([h (hash)]) ([tuple (in-vector tuples)])
                (define keyvalue (vector-ref tuple keyindex))
                (hash-set h keyvalue (cons tuple (hash-ref h keyvalue null))))]))
     (define lookups* (hash-set lookups keyindex (cons (and unique? #t) lookup)))
     (relation name heading tuples lookups*)]))

;; relation-index : Relation Symbol -> Nat/#f
(define (relation-index rel keyfield)
  (vector-index-of (relation-heading rel) keyfield))

;; in-relation : Relation Symbol/s -> Sequence
(define (in-relation rel wantfield/s #:who [who 'in-relation])
  (define project (make-project who rel wantfield/s))
  (sequence-map project (in-vector (relation-tuples rel))))

;; relation-find : Relation Symbol X [Bool] -> Vector/#f or (Listof Vector)
(define (relation-find rel keyfield keyvalue [all? #f] #:who [who 'relation-find])
  ((relation-prepare-find rel keyfield all? #:who who) keyvalue))

;; relation-ref : Relation Symbol X Symbol/s -> Y
(define (relation-ref rel keyfield keyvalue wantfield/s [default #f]
                      #:who [who 'relation-ref])
  ((relation-prepare-ref rel keyfield wantfield/s #:who who) keyvalue default))

;; relation-ref-all : Relation Symbol X Symbol/s -> (Listof Y)
(define (relation-ref-all rel keyfield keyvalue wantfield/s
                          #:who [who 'relation-ref-all])
  ((relation-prepare-ref-all rel keyfield wantfield/s #:who who) keyvalue))

;; relation-prepare-find : Relation Symbol [Bool] -> X -> Vector/#f or (Listof Vector)
(define (relation-prepare-find rel keyfield [all? #f]
                               #:who [who 'relation-prepare-find])
  (define keyindex (get-index who rel keyfield))
  (define find (make-find who rel keyindex all?))
  (cond [all? (lambda (keyvalue) (reverse (find keyvalue)))]
        [else find]))

;; relation-prepare-ref : Relation Symbol Symbol/s -> X -> Y
(define (relation-prepare-ref rel keyfield wantfield/s
                              #:who [who 'relation-prepare-ref])
  (define keyindex (get-index who rel keyfield))
  (define find (make-find who rel keyindex #f))
  (define project (make-project who rel wantfield/s))
  (define (ref keyvalue [default #f])
    (cond [(find keyvalue) => project]
          [else (if (procedure? default) (default) default)]))
  ref)

;; relation-prepare-ref-all : Relation Symbol Symbol/s -> X -> (Listof Y)
(define (relation-prepare-ref-all rel keyfield wantfield/s
                                  #:who [who 'relation-prepare-ref-all])
  (define keyindex (get-index who rel keyfield))
  (define find-all (make-find who rel keyindex #t))
  (define project (make-project who rel wantfield/s))
  (define (ref-all keyvalue)
    (for/fold ([acc null]) ([tuple (in-list (find-all keyvalue))])
      (cons (project tuple) acc)))
  ref-all)

;; get-index : Symbol Relation Symbol -> Nat
(define (get-index who rel field)
  (or (vector-index-of (relation-heading rel) field)
      (error who "field not in relation\n  relation: ~e\n  field: ~e" rel field)))

;; vector-index-of : (Vectorof X) X -> Nat/#f
(define (vector-index-of v k)
  (for/or ([e (in-vector v)] [index (in-naturals)])
    (if (equal? k e) index #f)))

;; make-find : Symbol Relation Nat Bool -> X -> Vector/#f or (Listof Vector)
(define (make-find who rel keyindex all?)
  (match-define (relation _ _ tuples lookups) rel)
  (cond [(hash-ref lookups keyindex #f)
         => (lambda (lookup)
              (match lookup
                [(cons #t luh)
                 (cond [all?
                        (lambda (keyvalue)
                          (cond [(hash-ref luh keyvalue #f) => list] [else null]))]
                       [else
                        (lambda (keyvalue)
                          (hash-ref luh keyvalue #f))])]
                [(cons #f luh)
                 (cond [all?
                        (lambda (keyvalue)
                          (hash-ref luh keyvalue null))]
                       [else
                        (lambda (keyvalue)
                          (define tuples (hash-ref luh keyvalue null))
                          (and (pair? tuples) (last tuples)))])]))]
        [all?
         (lambda (keyvalue)
           (for/fold ([acc null]) ([tuple (in-vector tuples)])
             (if (equal? (vector-ref tuple keyindex) keyvalue) (cons tuple acc) acc)))]
        [else
         (lambda (keyvalue)
           (for/first ([tuple (in-vector tuples)]
                       #:when (equal? (vector-ref tuple keyindex) keyvalue))
             tuple))]))

;; make-project : Symbol Relation Symbol/(Listof Symbol) -> Vector -> X/(Listof X)
(define (make-project who rel wantfield/s)
  (match-define (relation name heading _ _) rel)
  (cond [(list? wantfield/s)
         (define indexes
           (for/list ([wantfield (in-list wantfield/s)])
             (get-index who rel wantfield)))
         (lambda (tuple)
           (for/list ([index (in-list indexes)]) (vector-ref tuple index)))]
        [else
         (define index (get-index who rel wantfield/s))
         (lambda (tuple) (vector-ref tuple index))]))
