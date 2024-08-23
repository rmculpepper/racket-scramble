#lang scribble/manual
@(require scribble/example
          (for-label racket/base scramble/let-return))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require scramble/let-return)))

@; ----------------------------------------
@title[#:tag "let-return"]{Let-Return}

@defmodule[scramble/let-return]

@history[#:added "0.4"]

@defform[(let-return ([id rhs-expr] ...) body ...+)]{

Similar to @racket[(let ([id rhs-expr] ...) body ...+)], but discards the result
of the @racket[body] and returns @racket[(values id ...)] instead.

Useful for combining the creation and initialization of mutable objects into a
single expression.

@examples[#:eval the-eval
(define the-table
  (let-return ([h (make-hash)])
    (hash-set! h 'a 1)
    (hash-set! h 'b 2)))
the-table
]}

@(close-eval the-eval)
