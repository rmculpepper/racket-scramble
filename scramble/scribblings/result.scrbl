#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract racket/match scramble/result))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require racket/match scramble/result)))

@title[#:tag "result"]{Results}

@defmodule[scramble/result]

This module defines a @deftech{result} type. In general, a result is either
@racket[ok] and carries a success value, or it is @racket[bad] and carries a
value representing failure.

@examples[#:eval the-eval
(code:line (ok 5)             (code:comment "(Result Integer)"))
(code:line (ok (ok 5))        (code:comment "(Result (Result Integer))"))
(code:line (ok 'hello)        (code:comment "(Result Symbol)"))
(code:line (ok (list 1 2 3))  (code:comment "(Result (Listof Integer))"))
(code:line (ok (bad 123))     (code:comment "(Result (Result _ Integer))"))
]

@defstruct*[ok ([value any/c]) #:prefab]{

Struct type for wrapped OK results.
}

@defstruct*[bad ([value any/c]) #:prefab]{

Struct type for bad results.
}

@defproc[(result? [v any/c]) boolean?]{

Equivalent to @racket[(or (ok? v) (bad? v))].
}

@defproc[(result/c [ok-value/c contract?] [bad-value/c contract? any/c]) contract?]{

A value @racket[_result] is accepted by @racket[(result/c ok-value/c bad-value/c)] if
@itemlist[

@item{@racket[_result] is @racket[(ok _v)] and @racket[_v] is accepted by
@racket[ok-value/c], or}

@item{@racket[_result] is @racket[(bad _v)] and @racket[_v] is accepted by
@racket[bad-value/c].}

]}

@;{
@defform[(result-do ([id result-expr] ...) body ...+)
         #:contracts ([result-expr (result/c _X _Y)])]
@defproc[(result-bind [c (result/c _X _Z)]
                      [f (-> _X (result/c _Y _Z))]
                      [#:bad-f bad-f (or/c #f (-> _Z (result/c _Y _Z))) #f])
         (result/c _Y _Z)]

@defform[(result-and result-expr ...+)
         #:contracts ([result-expr (result/c _X _Y)])]{

Returns the first @racket[bad] result, or if all results are @racket[ok],
returns the last result.

Analogous to @racket[and], treating @racket[ok] results as true and @racket[bad]
results as false. Unlike @racket[and], it requires at least one argument.

@examples[#:eval the-eval
(result-and (ok 1) (ok 2) (ok 3))
(result-and (ok 1) (bad 'abc) (bad 'xyz) (ok 3))
]}

@defform[(result-or result-expr ...+)
         #:contracts ([result-expr (result/c _X _Y)])]{

Returns the first result that is @racket[ok], or if no results are @racket[ok],
returns the last result.

Analogous to @racket[or], treating @racket[ok] results as true and @racket[bad]
results as false. Unlike @racket[or], it requires at least one argument.

@examples[#:eval the-eval
(result-or (ok 1) (ok 2) (ok 3))
(result-or (ok 1) (bad 'abc) (ok 3))
(result-or (bad 'abc) (bad 'xyz))
]}

@defproc[(result-andmap [f (-> _X (result/c _Y _Z))]
                        [vs (non-empty-listof _X)])
         (result/c _Y _Z)]

@defproc[(result-ormap [f (-> _X (result/c _Y _Z))]
                       [vs (non-empty-listof _X _Z)])
         (result/c _Y _Z)]

}

@defproc[(partition-results [rs (listof (result/c _X _Y))])
         (values (listof _X) (listof _Y))]{

Returns the list of OK values and the list of bad values occurring in
@racket[rs]. The order of values is preserved.

@examples[#:eval the-eval
(partition-results (list (ok 1) (bad 2) (ok 3)))
]}


@; ----------------------------------------
@(close-eval the-eval)
