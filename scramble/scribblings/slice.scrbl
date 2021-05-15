#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract scramble/slice))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require scramble/slice)))


@; ----------------------------------------
@title[#:tag "slice"]{Slices}

@defmodule[scramble/slice]

@defstruct*[slice
            ([value (or/c bytes? string? vector? slice?)]
             [start exact-nonnegative-integer?]
             [end exact-nonnegative-integer?])]{

A slice represents a part of an underlying indexed collection (a byte string,
string, or vector).

The @racket[start] and @racket[end] fields must be appropriate for
@racket[value], and @racket[start] must be less than or equal to @racket[end];
otherwise, an exception is raised.

The @racket[slice] constructor performs the following adjustments:
@itemlist[

@item{If @racket[end] is @racket[#f], it is replaced with the length of the
given @racket[value]. That is @racket[slice-end] never returns @racket[#f].}

@item{If @racket[value] is a slice, then its value is extracted and
@racket[start] and @racket[end] are adjusted to refer to the underlying value.
That is, @racket[slice-value] never returns a slice.}

@item{If @racket[start] is equal to @racket[end], then @racket[value] is
replaced with the empty value of the same type, and @racket[start] and
@racket[end] are set to @racket[0].}

]

See @racket[print-slice-constructor-modes] for information about printing
slices.

Note: Future versions of this library may extend the set of types allowed as
@racket[value]s.
}

@deftogether[[
@defproc[(bytes-slice? [v any/c]) boolean?]
@defproc[(string-slice? [v any/c]) boolean?]
@defproc[(vector-slice? [v any/c]) boolean?]
]]{

Returns @racket[#t] if @racket[v] is a slice containing a byte string, string,
or vector, respectively; returns @racket[#f] otherwise.
}

@defparam[print-slice-constructor-modes modes (listof (or/c #t #f 0 1))
          #:value '(0 1)]{

Determines whether a slice is printed as a struct or as its contents.

When a slices is printed using a mode (see @racket[gen:custom-write]) in
@racket[modes], it is printed as a struct; otherwise, only its contents are
printed.

@examples[#:eval the-eval
(define s (slice (for/vector ([i 16]) i) 9 13))
(print s)
(write s)
(parameterize ((print-slice-constructor-modes '(#t)))
  (write s))
]}

@defproc[(slice-length [s slice?]) exact-nonnegative-integer?]{

Returns the length of the slice.

Equivalent to @racket[(- (slice-end s) (slice-start s))].
}

@defproc[(slice-contents [s slice?] [mutability (or/c 'mutable 'immutable #f)])
         (or/c bytes? string? vector?)]{

Returns a fresh copy of the slice's contents.

If @racket[mutability] is @racket['mutable], the result is mutable; if
@racket['immutable], the result is immutable; otherwise, the result is immutable
when the slice's underlying value is immutable.
}

@deftogether[[
@defproc[(bytes-slice->string/utf-8 [bs bytes-slice?] [err-char (or/c char? #f) #f])
         string?]
@defproc[(string->bytes/utf-8 [ss string-slice?])
         bytes?]
]]{

Wrappers for @racket[bytes->string/utf-8] and @racket[string->bytes/utf-8],
respectively, that obtain the value and indexes from the slice.

@examples[#:eval the-eval
(bytes-slice->string/utf-8 (slice #"hello world" 6 #f))
]}


@; ----------------------------------------
@(close-eval the-eval)
