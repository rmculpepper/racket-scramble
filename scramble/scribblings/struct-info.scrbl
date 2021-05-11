#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract racket/struct-info racket/match
                     scramble/struct-info))

@(begin
  (define the-eval (make-base-eval)))

@; ----------------------------------------
@title[#:tag "struct-info"]{Compile-time Struct Info}

@defmodule[scramble/struct-info]

@defproc[(adjust-struct-info [base-info struct-info?]
                             [#:constructor constructor-id (or/c identifier? #f) #f]
                             [#:match-expander match-expander (or/c #f (-> syntax? syntax?)) #f])
         struct-info?]{

Produces a struct-info record like @racket[base-info] but with new behavior as a
constructor and match pattern. That is, if @racket[_name] is bound to the result
using @racket[define-syntax] etc, and if @racket[constructor-id] is not
@racket[#f], then a use of @racket[_name] as an expression is redirected to
@racket[constructor-id]; and if @racket[match-expander] is not @racket[#f], then
a use of @racket[_name] in a @racket[match] pattern is expanded using
@racket[match-expander].

If @racket[base-info] is an instance of a struct type with any of the following
properties, the result has the same properties with the same values:
@racket[prop:struct-auto-info], @racket[prop:struct-field-info]. Other
properties of @racket[base-info] are not preserved.

The following example defines a @racket[point] struct and then replaces
@racket[point] with an adjusted struct info record that adds a contract and
makes one of the constructor arguments optional:
@examples[#:eval the-eval #:label #f
(module point racket/base
  (provide (struct-out point))
  (struct point (x y) #:transparent))
(module point-adjusted racket/base
  (require (for-syntax racket/base scramble/struct-info)
           racket/contract
           (rename-in 'point [point orig-point]))
  (define (make-point x [y 0]) (orig-point x y))
  (define-module-boundary-contract checked-make-point make-point
    (->* [real?] [real?] point?)
    #:name-for-blame point)
  (define-syntax point
    (adjust-struct-info (syntax-local-value #'orig-point)
                        #:constructor #'checked-make-point))
  (provide (struct-out point)))
(require 'point-adjusted)
(point 1 2)
(point 3)
(eval:error (point 'hello 'goodbye))
(struct point3 point (z) #:transparent)
(point3 1 2 3)
]}


@; ----------------------------------------
@(close-eval the-eval)
