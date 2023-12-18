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

@defproc[(string->integer [s string?]
                          [#:min min-value real? -inf.0]
                          [#:max max-value real? +inf.0])
         (or/c exact-integer? #f)]{

Returns the exact integer represented by @racket[s] if it is in the range
[@racket[min-value], @racket[max-value]]; otherwise, returns @racket[#f].

@examples[#:eval the-eval
(string->integer "123" #:min 0 #:max 100)
(string->integer "123" #:min 0 #:max 500)
(code:line (string->integer "1e3") (code:comment "exponentials are not allowed"))
(code:line (string->integer "#x10") (code:comment "radix markers are not allowed"))
]

@history[#:added "0.4"]}

@deftogether[[
@defthing[MIN-INT8   exact-integer? #:value (- (expt 2 7))]
@defthing[MIN-INT16  exact-integer? #:value (- (expt 2 15))]
@defthing[MIN-INT32  exact-integer? #:value (- (expt 2 31))]
@defthing[MIN-INT64  exact-integer? #:value (- (expt 2 63))]
@defthing[MIN-INT128 exact-integer? #:value (- (expt 2 127))]
@defthing[MIN-INT256 exact-integer? #:value (- (expt 2 255))]
]]{

Minimum integer representable as signed (two's complement) in 8, 16, 32, 64,
128, and 256 bits, respectively.

@history[#:added "0.4"]
}

@deftogether[[
@defthing[MAX-INT8   exact-integer? #:value (sub1 (expt 2 7))]
@defthing[MAX-INT16  exact-integer? #:value (sub1 (expt 2 15))]
@defthing[MAX-INT32  exact-integer? #:value (sub1 (expt 2 31))]
@defthing[MAX-INT64  exact-integer? #:value (sub1 (expt 2 63))]
@defthing[MAX-INT128 exact-integer? #:value (sub1 (expt 2 127))]
@defthing[MAX-INT256 exact-integer? #:value (sub1 (expt 2 255))]
]]{

Maximum integer representable as signed (two's complement) in 8, 16, 32, 64,
128, and 256 bits, respectively.

@history[#:added "0.4"]
}

@deftogether[[
@defthing[MAX-UINT8   exact-integer? #:value (sub1 (expt 2 16))]
@defthing[MAX-UINT16  exact-integer? #:value (sub1 (expt 2 16))]
@defthing[MAX-UINT32  exact-integer? #:value (sub1 (expt 2 32))]
@defthing[MAX-UINT64  exact-integer? #:value (sub1 (expt 2 64))]
@defthing[MAX-UINT128 exact-integer? #:value (sub1 (expt 2 128))]
@defthing[MAX-UINT256 exact-integer? #:value (sub1 (expt 2 256))]
]]{

Maximum integer representable as unsigned in 8, 16, 32, 64, 128, and 256 bits,
respectively.

@history[#:added "0.4"]
}

@deftogether[[
@defproc[(int8?   [v any/c]) boolean?]
@defproc[(int16?  [v any/c]) boolean?]
@defproc[(int32?  [v any/c]) boolean?]
@defproc[(int64?  [v any/c]) boolean?]
@defproc[(int128? [v any/c]) boolean?]
@defproc[(int256? [v any/c]) boolean?]
]]{

Returns @racket[#t] if @racket[v] is an exact integer representable as signed
(two's complement) using 8, 16, 32, 64, 128, or 256 bits, respectively;
otherwise, returns @racket[#f].

@history[#:added "0.4"]
}

@deftogether[[
@defproc[(uint8?   [v any/c]) boolean?]
@defproc[(uint16?  [v any/c]) boolean?]
@defproc[(uint32?  [v any/c]) boolean?]
@defproc[(uint64?  [v any/c]) boolean?]
@defproc[(uint128? [v any/c]) boolean?]
@defproc[(uint256? [v any/c]) boolean?]
]]{

Returns @racket[#t] if @racket[v] is an exact nonnegative integer representable
as unsigned using 8, 16, 32, 64, 128, or 256 bits, respectively; otherwise,
returns @racket[#f].

@history[#:added "0.4"]
}

@; ----------------------------------------
@(close-eval the-eval)
