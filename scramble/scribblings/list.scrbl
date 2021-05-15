#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract racket/shared scramble/list))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require racket/shared scramble/list)))

@; ----------------------------------------
@title[#:tag "list"]{Lists}

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
@(close-eval the-eval)
