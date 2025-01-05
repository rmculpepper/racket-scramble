#lang scribble/manual
@(require (for-syntax racket/base)
          scribble/example
          (only-in scribble/racket make-element-id-transformer)
          (for-label racket/base racket/contract
                     scramble/inject-syntax))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require scramble/datum-to-expr (only-in racket/base [quote Quote]))))

@; ----------------------------------------
@title[#:tag "datum-to-expr"]{Converting a Datum to an Expression}

@defmodule[scramble/datum-to-expr]

@history[#:added "0.5"]

@defproc[(datum->expression [v any/c]
                            [#:convert convert (-> any/c (U #f syntax?)) (lambda (v) #f)])
         syntax?]{

Converts the datum @racket[v] into an expression that evaluates to a value
equivalent to @racket[v]. This function is useful for a macro that has a
(compile-time) value and wants it to be recreated at run time, or at compile
time after the macro ends, but the value contains syntax objects, so it cannot
simply be wrapped in a @racket[quote] expression. The expression is suitable for
evaluation at either run time or compile time.

The types of values directly supported are the quotable atomic values
(@racket[null], booleans, numbers, charactes, strings, byte strings, symbols,
keywords, and regexps), pairs, vectors, boxes, immutable prefab structs, hashes,
and syntax objects. Other types of data can be supported via the optional
@racket[convert] argument. The @racket[convert] function is called with a value
that is not one of the types listed above, and it must either produce either
@racket[#f] or a syntax object representing an expression to recreate the value.

The value that the expression produces is equivalent to @racket[v] with the
following exceptions: mutable data (strings, byte strings, vectors, boxes, and
hashes) are converted into immutable variants; syntax objects may gain scopes
and may lose syntax properties due to serialization. Sharing is generally not
preserved.
}

@; ------------------------------------------------------------
@; @(close-eval the-eval)
