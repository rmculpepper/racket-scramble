#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract scramble/number))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require scramble/number)))

@; ----------------------------------------
@title[#:tag "number"]{Numbers}

@defmodule[scramble/number]

@deftogether[[
@defproc[(exact [r number?]) number?]
@defproc[(inexact [r number?]) number?]
]]{

Aliases for @racket[inexact->exact] and @racket[exact->inexact], respectively.
}

@deftogether[[
@defproc[(min* [r real?] ...) real?]
@defproc[(max* [r real?] ...) real?]
]]{

Like @racket[min] and @racket[max], respectively, but they return the
appropriate infinity if given zero arguments, and comparisons against
@racket[+inf.0] and @racket[-inf.0] do not coerce the result to inexact.

@examples[#:eval the-eval
(code:line (min 1 +inf.0) (code:comment "normal min"))
(min* 1 +inf.0)
(min* 1 2.0)
(max*)
]}

@defproc[(ceiling-quotient [n exact-integer?] [d exact-positive-integer?])
         exact-integer?]{

Equivalent to @racket[(ceiling (/ n d))], but avoids the intermediate rational
result.

@examples[#:eval the-eval
(ceiling-quotient 7 4)
(ceiling-quotient 8 4)
(ceiling-quotient 9 4)
@;{
(code:comment "on negative numbers")
(ceiling-quotient -6 5)
(ceiling-quotient -5 5)
(ceiling-quotient -4 5)
}
]}

@defproc[(ceiling-multiple [n exact-integer?] [d exact-positive-integer?])
         exact-integer?]{

Returns the least multiple of @racket[d] greater than or equal to @racket[n].

Equivalent to @racket[(* d (ceiling-quotient n d))].

@examples[#:eval the-eval
(ceiling-multiple 7 4)
(ceiling-multiple 8 4)
(ceiling-multiple 9 4)
@;{
(code:comment "on negative numbers")
(ceiling-multiple -6 5)
(ceiling-multiple -5 5)
(ceiling-multiple -4 5)
}
]}

@deftogether[[
@defproc[(floor-quotient [n exact-integer?] [d exact-positive-integer?])
         exact-integer?]
@defproc[(floor-multiple [n exact-integer?] [d exact-positive-integer?])
         exact-integer?]
]]{

Equivalent to @racket[(floor (/ n d))] and @racket[(* d (floor-quotient n d))],
respectively.
}


@; ----------------------------------------
@(close-eval the-eval)
