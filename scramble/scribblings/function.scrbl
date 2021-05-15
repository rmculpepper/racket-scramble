#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract racket/function
                     scramble/function))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require scramble/function)))

@; ----------------------------------------
@title[#:tag "function"]{Functions}

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
@(close-eval the-eval)
