#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract scramble/immutable))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require scramble/immutable)))

@; ----------------------------------------
@title[#:tag "immutable"]{Immutable and Mutable Conversion}

@defmodule[scramble/immutable]

@defproc[(immutable [v any/c]) any/c]{

Returns a value like @racket[v] that is shallowly immutable (see
@racket[immutable?]). The argument must be a vector, string, or other type of
data with both mutable and immutable variants. (Mutable pairs are not considered
a mutable variant of pairs.)

The result is immutable, and it is not an impersonator. If @racket[v] is a
suitable result, it is returned as the result; that is, the result of the
function may not be a fresh value.

Note that if @racket[v] is impersonated, this function can raise an exception
due to accessing @racket[v].
}

@defproc[(mutable [v any/c] [fresh? boolean?]) any/c]{

Like @racket[immutable], but returns a value that is shallowly mutable and not
an impersonator.

If @racket[fresh?] is true, then the result is a new value that does not share
storage (shallowly) with @racket[v].

Note that if @racket[v] is impersonated, this function can raise an exception
due to accessing @racket[v].
}


@; ----------------------------------------
@(close-eval the-eval)
