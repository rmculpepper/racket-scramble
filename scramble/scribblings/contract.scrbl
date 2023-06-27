#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract scramble/contract))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require racket/contract scramble/contract)))

@; ----------------------------------------
@title[#:tag "contract"]{Contracts}

@defmodule[scramble/contract]

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
}

@; ----------------------------------------
@(close-eval the-eval)
