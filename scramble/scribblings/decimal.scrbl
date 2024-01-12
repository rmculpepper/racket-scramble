#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract racket/math scramble/decimal))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require racket/math scramble/decimal)))

@; ----------------------------------------
@title[#:tag "decimal"]{Decimal Numbers}

@defmodule[scramble/decimal]

@history[#:added "0.4"]

@defstruct*[decimal ([unscaled exact-integer?] [scale exact-integer?])]{

Represents an exact decimal number as an integer and a scale. The number
represented is @racket[(/ unscaled (expt 10 scale))]; for example,
@racket[(decimal 12345 3)] represents the number @racket[12.345].

The @racket[scale] value is also called @emph{precision} throughout this
document. When non-negative, it corresponds to the number of places after the
decimal point. When negative, it indicates the number of places before the
decimal point that are insignificant.
}

@defproc[(real->decimal [x rational?] [precision exact-integer?])
         decimal?]{

Converts @racket[x] to a decimal with the given precision. If @racket[x] cannot
be represented exactly with the given precision, it is @racket[round]ed. If
@racket[x] is a special real, such as @racket[+inf.0] or @racket[+nan.0], an
exception is raised.

@examples[#:eval the-eval #:label #f
(real->decimal 1/8 4)
(real->decimal 1/3 3)
]}

@defproc[(string->decimal [s string?])
         (or/c decimal? #f)]{

Returns the decimal number represented by @racket[s], or @racket[#f] if
@racket[s] does not contain a decimal number. The acceptable formats are
@racket[#rx"-?[0-9]*[.][0-9]+"] and @racket[#rx"-?[0-9]+#*[.]?"]. The resulting
decimal's scale is determined either by the number of digits after the decimal
point or the number of @litchar{#} markers before the decimal point; the latter
indicate negative scale.

@examples[#:eval the-eval #:label #f
(string->decimal "123.45")
(string->decimal ".98")
(string->decimal "-22.44")
(string->decimal "123##")
(string->decimal "-.")
]}

@defproc[(decimal->string [dec decimal?])
         string?]{

Converts the decimal @racket[dec] into a string. If @racket[dec] has negative
scale, the insignificant digits are indicated with @litchar{#}.

@examples[#:eval the-eval #:label #f
(decimal->string (decimal 12345 2))
(decimal->string (decimal 25 -3))
]}

@deftogether[[
@defproc[(decimal->exact [dec (or/c decimal? rational?)]) (and/c rational? exact?)]
@defproc[(decimal->inexact [dec (or/c decimal? real?)]) (and/c real? inexact?)]
]]{

Converts the given decimal into an ordinary exact or inexact number,
respectively. For convenience, both functions also accept ordinary real numbers.

@examples[#:eval the-eval
(decimal->exact (string->decimal "123.45"))
(decimal->exact (string->decimal "987"))
(decimal->exact 1/4)
(decimal->inexact (string->decimal "123.45"))
(decimal->inexact +inf.0)
]}

@defproc[(decimal-adjust [dec decimal?] [precision exact-integer?])
         decimal?]{

Returns a new decimal with the given @racket[precision] as close as possible to
@racket[dec]. If the given @racket[precision] is less than the precision (scale)
of @racket[dec], the decimal is @racket[round]ed.

@examples[#:eval the-eval
(decimal-adjust (string->decimal "123.456") 5)
(decimal-adjust (string->decimal "123.456") 1)
(decimal-adjust (string->decimal "12.5") 0)
(decimal-adjust (string->decimal "13.5") 0)
(decimal-adjust (string->decimal "123456") -2)
]}

@deftogether[[
@defproc[(decimal+ [v (or/c decimal? real?)] ...)  (or/c decimal? real?)]
@defproc[(decimal- [v (or/c decimal? real?)] ...+) (or/c decimal? real?)]
@defproc[(decimal* [v (or/c decimal? real?)] ...)  (or/c decimal? real?)]
]]{

Returns the sum, difference, or product, respectively, of the given decimals.

The precision of an operation involving two decimals is the max of the operand
precisions for addition and subtraction, and the sum of the precisions for
multiplications. The precision of an operation involving a decimal and an
ordinary real number is the precision of the decimal. Operations are performed
left to right.

@bold{Warning: } Arithmetic on mixtures of reals and decimals is neither
associative nor generally commutative, due to the precision rules above. For
example:

@examples[#:eval the-eval #:label #f
(decimal+ (string->decimal "1.00") pi (string->decimal "0.0001"))
(decimal+ (string->decimal "0.0001") pi (string->decimal "1.00"))
]
}

@defproc[(decimal-cmp [v1 (or/c decimal? real?)] [v2 (or/c decimal? real?)])
         (or/c '< '= '> #f)]{

Compares the numeric values of the given decimals and/or reals.

@examples[#:eval the-eval
(decimal-cmp (string->decimal "2.0") (string->decimal "2.000"))
(decimal-cmp (string->decimal "1.000") 1)
(decimal-cmp (string->decimal "123.45") 100)
(decimal-cmp (string->decimal "999") +inf.0)
]
}

@deftogether[[
@defproc[(decimal<?  [v (or/c decimal? real?)] ...+) boolean?]
@defproc[(decimal<=? [v (or/c decimal? real?)] ...+) boolean?]
@defproc[(decimal=?  [v (or/c decimal? real?)] ...+) boolean?]
@defproc[(decimal>?  [v (or/c decimal? real?)] ...+) boolean?]
@defproc[(decimal>=? [v (or/c decimal? real?)] ...+) boolean?]
]]{

Compares the chain of the numeric values of the given decimals and/or reals.
}

@deftogether[[
@defproc[(decimal-min [v (or/c decimal? real?)] ...+) (or/c decimal? real?)]
@defproc[(decimal-max [v (or/c decimal? real?)] ...+) (or/c decimal? real?)]
]]{

Returns the minimum or maximum, respectively, of the given decimals and/or reals.
}

@defproc[(decimal-abs [dec decimal?]) decimal?]{

Returns the absolute value of the given decimal.
}


@; ----------------------------------------
@(close-eval the-eval)
