#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract scramble/contract))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require racket/contract scramble/contract)))

@; ----------------------------------------
@title[#:tag "contract"]{Contracts}

@defmodule[scramble/contract]

@history[#:added "0.4"]

@deftogether[[
@defproc[(vectorof/ic [elem/c contract?])
         impersonator-contract?]
@defproc[(hash/ic [key/c contract?]
                  [value/c contract?])
         impersonator-contract?]
@defthing[string/ic impersonator-contract?]
@defthing[bytes/ic impersonator-contract?]
]]{

Like the contracts @racket[(vectorof elem/c)], @racket[(hash/c key/c elem/c)],
@racket[string?], and @racket[bytes?], except that the result of the contract's
projection is always immutable and authentic (not a chaperone or impersonator).

@examples[#:eval the-eval
(define the-vector (vector))
(define/contract install!
  (-> (vectorof/ic real?) void?)
  (lambda (v) (set! the-vector v)))
(define v (vector 1 2 3))
(install! v)
(vector-set! v 0 'apple)
(vector-set! v 1 'banana)
(eq? the-vector v)
(vector-ref the-vector 0)
]

If the original value is suitable, the contract projection returns it unchanged.

@examples[#:eval the-eval #:label #f
(define imm-vec (quote #(1 2 3)))
(install! imm-vec)
(eq? the-vector imm-vec)
]

The value produced by a @racket[hash/ic] contract uses the same key comparison
as the original value.

@examples[#:eval the-eval #:label #f
(define/contract hash-snapshot
  (-> (hash/ic any/c any/c) any)
  (lambda (h) h))
(hash-snapshot (make-hasheqv '((1 . "one") (2 . two))))
(hash-snapshot (hash '(1 2 3) 6 '(4 5) 9))
]
}

@defproc[(hash-record/ic [key=>value/c (hash/c any/c contract?)]
                         [#:name name any/c '(hash-record/ic ....)]
                         [#:other get-other/c (-> any/c contract?) (lambda (_key) any/c)]
                         [#:required required (or/c 'all list?) 'all])
         impersonator-contract?]{

Produces a contract that accepts hashes, treating them like records and
enforcing specific contracts on each of their fields. Specifically, when the
contract is applied to a hash, for each @racket[_key] and @racket[_value] in the
hash, the contract associated with @racket[_key] by @racket[key=>value/c] is
applied to @racket[value]. If @racket[key=>value/c] has no entry for
@racket[_key], then the value contract is obtained from @racket[(get-other/c
_key)] instead. The final result of the contract's projection is immutable and
authentic (not a chaperone or impersonator).

If @racket[required] is @racket['all], then every key of @racket[key=>value/c]
is required. If @racket[required] is a list, each element must be a key of
@racket[key=>value/c], and only those keys are considered required.

@examples[#:eval the-eval #:label #f
(define point/c (hash-record/ic (hash 'x real? 'y real?)))
(define/contract point-taxi-dist
  (-> point/c real?)
  (lambda (p) (+ (abs (hash-ref p 'x)) (abs (hash-ref p 'y)))))
(point-taxi-dist (hash 'x 5 'y 3))
(point-taxi-dist (hash 'x 5 'y 3 'elevation -10))
(eval:error (point-taxi-dist (hash 'x 5 'y 'north)))
(eval:error (point-taxi-dist (hash 'x 5)))
]

@history[#:added "0.6"]}

@defproc[(convert/ic [convert (-> any/c any/c)]
                     [#:name name any/c '(convert/ic ....)]
                     [#:exn->lines exn->lines (lambda (exn) ....)])
         impersonator-contract?]{

Produces a contract that applies @racket[convert] to the given value. If the
call to @racket[convert] produces a value, that is the value produced by the
contract. If the call to @racket[convert] raises an exception, then the contract
rejects the given value and the contract error includes a summary of the
exception (determined by @racket[exn->lines]).

The @racket[convert] procedure should be idempotent; that is, @racket[(convert
(convert _value))] should be equal to @racket[(convert _value)].

The @racket[exn->lines] function must produce a string suitable for adding to
the end of a well-formed error message, such as @racket[""] or
@racket["\n error: went wrong"]. The default @racket[exn->lines] produces
@racket["\n error: "] followed by the first line of the exception's message,
plus @racket["..."] if the message was truncated.

@examples[#:eval the-eval
(define (to-string v)
  (cond [(string? v) v]
        [(bytes? v) (bytes->string/utf-8 v)]
        [(symbol? v) (symbol->string v)]
        [else (error 'to-string "cannot convert to string\n  given: ~e" v)]))
(define stringable/ic
  (convert/ic to-string #:name 'stringable/ic))
(define/contract concat (-> (listof stringable/ic) string?)
  (lambda (strs) (apply string-append strs)))
(concat (list "a" #"b" 'c))
(eval:error (concat (list "a" #"b" 5)))
]}


@; ----------------------------------------
@(close-eval the-eval)
