#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract scramble/struct))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require scramble/struct)))


@; ----------------------------------------
@title[#:tag "struct"]{Structs}

@defmodule[scramble/struct]

@defthing[prop:auto-equal+hash
          (struct-type-property/c
           (or/c #t (listof exact-nonnegative-integer?)))]{

Property that automatically adds an implementation of the
@racket[prop:equal+hash] property. If the property value is @racket[#t], the
automatically generated equality predicate and hash code functions use all of
the struct type's fields. If the property value is a list, then the equality
predicate and hash code functions use only the fields with the indexes in the
list.

If @racket[prop:auto-equal+hash] is attached to a struct type that has a super
struct type, then the super struct type must also have the
@racket[prop:auto-equal+hash], and the new equality and hash code functions
extend the super type's functions. If the super struct type does not have the
@racket[prop:auto-equal+hash] property, an error is raised.

@examples[#:eval the-eval #:label #f
(struct point (x y)
  #:property prop:auto-equal+hash #t)
(equal-hash-code (point 1 2))
(equal? (point 1 2) (point 1 2))
(equal? (point 1 2) (point 0 0))
]

In the following example, the equality and hash code functions of the
@racket[point3] struct type use only the @racket[z] field out of
@racket[point3]'s fields, disregarding the @racket[color] field, but also use
both of @racket[point]'s fields.

@examples[#:eval the-eval #:label #f
(struct point3 point (z color)
  #:property prop:auto-equal+hash (list (struct-field-index z)))
(equal? (point3 1 2 3 #f)
        (point3 1 2 3 'red))
(equal? (point3 0 0 3 'red)
        (point3 1 2 3 'red))
(equal? (equal-hash-code (point3 1 2 3 #f))
        (equal-hash-code (point3 1 2 3 'red)))
]}


@; ----------------------------------------
@(close-eval the-eval)
