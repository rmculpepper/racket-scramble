#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract scramble/cond))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require scramble/cond)))

@; ----------------------------------------
@title[#:tag "cond"]{Conditional Expressions}

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
@(close-eval the-eval)
