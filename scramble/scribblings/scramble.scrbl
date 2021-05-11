#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract racket/class
                     racket/function racket/shared racket/struct
                     racket/require racket/symbol
                     scramble/class
                     scramble/cond
                     scramble/function
                     scramble/immutable
                     scramble/list
                     scramble/number
                     scramble/slice
                     scramble/about
                     ))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require scramble/class racket/class racket/shared
                      scramble/cond
                      scramble/function
                      scramble/immutable
                      scramble/list
                      scramble/number
                      scramble/slice
                      scramble/about
                      )))

@title{scramble: Assorted Utility Libraries}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]


@; ----------------------------------------
@section[#:tag "cond"]{Conditional Expressions}

@defmodule[scramble/cond]

@defform[#:literals (else =>)
         (cond+ clause ... maybe-final-clause)
         #:grammar
         ([clause [test-expr then-body ...+]
                  [test-expr => proc-expr]
                  (code:line #:do defn-or-expr)]
          [maybe-final-clause (code:line)
                              [else then-body ...+]
                              (code:line #:else then-body ...+)])]{

Like @racket[cond], but allows addititional forms of clauses and raises an error
if no clause is selected.

The additional forms of @racket[clause] are allowed:
@specform[(code:line #:do defn-or-expr)]{

If the preceding clauses were not selected, evaluate @racket[defn-or-expr]
before continuing. If @racket[defn-or-expr] is a definition, the subsequent
clauses are in its scope.

Separate @racket[#:do] clauses have ``@racket[let*]'' scoping. That is, each
@racket[#:do] clause's form is evaluated in a separate internal definition
context nested within the previous scopes. Use @racket[begin] to group mutually
recursive definitions.
}

@specform[(code:line #:else then-body ...+)]{

Allowed only at the end. Equivalent to @racket[[else then-body ...]].
}

@examples[#:eval the-eval
(define entries
  `([x 5]
    [y ,(lambda (key) (list key 12))]))
(define (lookup key)
  (define entry (assoc key entries))
  (cond+ [(not entry) (error "not found")]
         #:do (define v (cadr entry))
         [(procedure? v) (v key)]
         #:else v))
(lookup 'x)
(lookup 'y)
(eval:error (lookup 'z))
(eval:error
 (cond+ [(odd? 12) 'odd]
        [(even? 7) 'even-odder]))
]}

@deftogether[[
@defform[(and+ part ... expr)
         #:grammar ([part expr (code:line #:do defn-or-expr)])]
@defform[(or+ part ... expr)
         #:grammar ([part expr (code:line #:do defn-or-expr)])]
]]{

Like @racket[and] and @racket[or], respectively, but allowing @racket[#:do]
clauses among the expressions. The @racket[part]s must be followed by a final
expression---that is, unlike @racket[and] and @racket[or], the arguments cannot
be empty.

@examples[#:eval the-eval
(and+ (even? 42)
      #:do (define x (quotient 42 2))
      (even? x))
]}


@; ----------------------------------------
@section[#:tag "function"]{Functions}

@defmodule[scramble/function]

@defproc[(K [v any/c] ...) procedure?]{

Returns a constant function that produces @racket[(values v ...)] when applied.

The resulting constant function's arity is @racket[(arity-at-least 0)]; that is,
it accepts (and discards) any number of positional arguments, but it does not
accept keyword arguments. See @racket[const] for a single-valued version whose
result accepts keyword arguments.

@examples[#:eval the-eval
(define always-zero (K 0))
(map always-zero '(1 2 3))
(define two-falses (K #f #f))
(define-values (head tail)
  (with-handlers ([exn? two-falses])
    (values (car '()) (cdr '()))))
]}

@defproc[(K0 [v any/c] ...) procedure?]{

Like @racket[K], but the resulting constant function's arity is 0 (with no
keywords).
}

@defproc[(call [f procedure?] [v any/c] ...) any]{

Calls @racket[f] on the arguments @racket[v ...]; equivalent to @racket[(f v
...)]. Primarily useful as an argument to higher-order functions.

@examples[#:eval the-eval
(define the-callbacks
  (list (lambda () (printf "here I am\n"))
        (lambda () (printf "me too!\n"))))
(map call the-callbacks)
]}


@; ----------------------------------------
@section[#:tag "immutable"]{Immutable and Mutable Conversion}

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
@section[#:tag "list"]{Lists}

@defmodule[scramble/list]

@defproc[(singleton? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a list of length 1; @racket[#f] otherwise.

Equivalent to @racket[(and (pair? v) (null? (cdr v)))].

@examples[#:eval the-eval
(singleton? (list 'hello))
(singleton? (list 1 2 3))
(singleton? (shared ([whys (cons 'why whys)]) whys))
]
}


@; ----------------------------------------
@section[#:tag "number"]{Numbers}

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
@section[#:tag "slice"]{Slices}

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
@section[#:tag "about"]{About Descriptions}

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
@section[#:tag "class"]{Classes and Objects}

@defmodule[scramble/class]

@defform[(init-private init-decl ...)]{

Like @racket[init-field], but declares private fields instead of public
fields. That is, the fields are declared via @racket[define] rather than
@racket[field]; the corresponding @racket[init]-argument names are still
externally visible.

@examples[#:eval the-eval
(define person%
  (class object%
    (init-field name)
    (init-private [id #f])
    (super-new)))
(define alice (new person% (name "Alice") (id 1001)))
(get-field name alice)
(eval:error (get-field id alice))
]}


@definterface[constructor-style-printable<%> ()]{

Interface for objects that implement constructor-style printing---that is, as a
@racket[new] expression with the class name and a sequence of initialization
arguments (@racket[init] arguments).

If the object's state is not representable in terms of its actual @racket[init]
arguments, then consider using @racket[printable<%>] to implement a printer that
does not produce constructor-style output. Or alternatively, also implement the
(empty) interface @racket[print-quotable-always<%>] to force printing in
non-expression style.

See also @racket[prop:custom-write] and @racket[make-constructor-style-printer].

@defmethod[(get-printing-class-name) symbol?]{

Returns a symbol to be used as the class name when printed.
}

@defmethod[(get-printing-components)
           (values (listof symbol?)
                   (listof any/c)
                   boolean?)]{

Returns @racket[(values _names _values _more?)], which control the printing of
the object's contents as follows:

The @racket[_names] represent the names of the object's fields---or more
properly, its @racket[init] arguments. The @racket[_values] are the
corresponding values; the two lists should have the same length.

If @racket[_more?] is true, then @litchar{...} is printed after the
initialization arguments to indicate that the object contains additional state
not represented by the printed output.
}

@examples[#:eval the-eval
(define friendly-person%
  (class* person% (constructor-style-printable<%>)
    (inherit-field name)
    (super-new)

    (define/public (get-printing-class-name)
      'friendly-person%)
    (define/public (get-printing-components)
      (values '(name) (list name) #t))))

(define bob (new friendly-person% (name "Bob")))
bob
(list bob)
(write bob)
]}

@definterface[print-quotable-never<%> ()]{

This interface contains no methods. It attaches the
@racket[prop:custom-print-quotable] property to objects, with the value
@racket['always].

@examples[#:eval the-eval
(define expressive-person%
  (class* friendly-person% (print-quotable-never<%>)
    (super-new)
    (define/override (get-printing-class-name)
      'expressive-person%)
    ))

(define kristen (new expressive-person% (name "Kristen")))
kristen
(list kristen)
(write kristen)
]}

@definterface[print-quotable-always<%> ()]{

This interface contains no methods. It attaches the
@racket[prop:custom-print-quotable] property to objects, with the value
@racket['always].

@examples[#:eval the-eval
(define obscure-person%
  (class* friendly-person% (print-quotable-always<%>)
    (super-new)
    (define/override (get-printing-class-name)
      'obscure-person%)
      ))

(define jeff (new obscure-person% (name "Jeff")))
jeff
(list jeff)
(write jeff)
]}


@definterface[about<%> ()]{

This interface attaches the @racket[prop:about] property to objects with an
implementation that calls the @method[about<%> about] method.

@defmethod[(about) string?]{

Implements the @racket[about] operation.
}

}


@; ----------------------------------------

@include-section["inject-syntax.scrbl"]
@include-section["struct-info.scrbl"]

@; ----------------------------------------
@(close-eval the-eval)
