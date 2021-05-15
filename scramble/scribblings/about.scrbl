#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract scramble/about scramble/class))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require scramble/about)))


@; ----------------------------------------
@title[#:tag "about"]{About Descriptions}

@defmodule[scramble/about]

The @racket[about] interface is useful for displaying a partial description of a
value in a situation where simply printing or writing the value would be
inappropriate. See also @racket[about<%>].

@defproc[(about [v any/c]) string?]{

Returns a string describing @racket[v], if @racket[v] implements the
@racket[prop:about] interface; otherwise, if @racket[(has-about? v)] is false,
then the string @racket["(no description)"] is returned.

@examples[#:eval the-eval
(struct secret (bs)
  #:property prop:about
  (lambda (self) (format "~s-bit secret" (* 8 (bytes-length (secret-bs self))))))
(define my-secret (secret #"hello"))
(eval:error
 (error 'secure-op "secret is too short\n  given: ~a" (about my-secret)))
(about 'apple)
]}

@defthing[prop:about
          (struct-type-property/c (-> any/c string?))]{

Property for structs to implement the @racket[about] operation.
}

@defproc[(has-about? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an instance of a struct type that
implements the @racket[prop:about] interface, @racket[#f] otherwise.
}


@; ----------------------------------------
@(close-eval the-eval)
