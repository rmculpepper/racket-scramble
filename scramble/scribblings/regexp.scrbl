#lang scribble/manual
@(require scribble/example
          (for-syntax racket/base)
          (only-in scribble/racket make-element-id-transformer)
          (for-label (except-in racket/base or not * +)
                     (except-in racket/contract any)
                     scramble/regexp))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require scramble/regexp)))

@title[#:tag "regexp"]{Regular Expressions}

@(define-syntax !RE (make-element-id-transformer (lambda (stx) #'(svar RE))))

@defmodule[scramble/regexp]

@history[#:added "0.3"]

This module works with the following S-expression representation of regular
expressions. All literals in the grammar are recognized as symbols, not by
binding.

@racketgrammar*[
#:literals (or cat repeat * + ? report any ^ $ mode test unicode chars not inject
                     union intersect complement look look-back matched?)
 [RE (code:line RE-id)
     (code:line (or RE ...+)                 (code:comment "like <RE>|<RE>"))
     (code:line (cat RE ...)                 (code:comment "like <RE><RE>"))
     (code:line (repeat RE)                  (code:comment "like <RE>*"))
     (code:line (repeat RE n)                (code:comment "like <RE>{n}"))
     (code:line (repeat RE m n)              (code:comment "like <RE>{m,n}"))
     (code:line (* RE)                       (code:comment "like <RE>*"))
     (code:line (+ RE)                       (code:comment "like <RE>+"))
     (code:line (? RE)                       (code:comment "like <RE>?"))
     (code:line (report RE)                  (code:comment "like (<RE>)"))
     (code:line (any)                        (code:comment "like ."))
     (code:line ^                            (code:comment "like ^"))
     (code:line $                            (code:comment "like $"))
     (code:line (mode modes-string RE)       (code:comment "like (?<modes>:<RE>)"))
     (code:line (test tst RE)                (code:comment "like (?<tst><RE>)"))
     (code:line (test tst RE RE)             (code:comment "like (?<tst><RE>|<RE>)"))
     (code:line (unicode prop-string)        (code:comment "like \\p{<prop>}"))
     (code:line (unicode (not prop-string))  (code:comment "like \\P{<prop>}"))
     (code:line (chars CharSet ...)          (code:comment "like [<CharSet>]"))
     (code:line Look)
     (code:line literal-string)
     (code:line (inject pregexp-string))]
 [CharSet (code:line (union CharSet ...))
          (code:line (intersect CharSet ...))
          (code:line (complement CharSet ...))
          (code:line chars-string)
          (code:line CharRange                    (code:comment "eg, [#\\A #\\Z]"))
          (code:line char/integer                 (code:comment "eg, #\\A, 65"))
          (code:line RE-id                        (code:comment "if value is CharSet"))
          (code:line posix-charset-id             (code:comment "eg, alpha, space"))]
 [CharRange [lo:char/integer hi:char/integer]]
 [Test Look
       (matched? n)]
 [Look (code:line (look RE)                    (code:comment "like (?=<RE>)"))
       (code:line (look (not RE))              (code:comment "like (?!<RE>)"))
       (code:line (look-back RE)               (code:comment "like (?<=<RE>)"))
       (code:line (look-back (not RE))         (code:comment "like (?<!<RE>)"))]]

@history[#:changed "0.5" @elem{Added @racket[any].}]

The forms of @svar[RE] should mostly be self-explanatory, but a few of them
deserve additional comments:

@specsubform[RE-id]{

If @racket[RE-id] was defined using @racket[define-RE], then its @svar[RE] value
is inserted in place of @racket[RE-id]; otherwise, a syntax error is raised.

If an @racket[RE-id] is defined with the same name as one of the unparenthesized
@svar[RE] forms (namely, @racket[^] or @racket[$]) or one of the POSIX character
classes (eg, @racket[alpha]), then the @racket[RE-id] takes precedence.
}

@specsubform[#:literals (repeat) (repeat RE m n)]{

Matches @racket[RE] between @racket[m] and @racket[n] times (inclusive), where
@racket[m] must be a natural number and @racket[n] must be a natural number or
@racket[+inf.0].

@itemlist[
@item{@racket[(repeat RE n)] is equivalent to @racket[(repeat RE n n)]}
@item{@racket[(* RE)] and @racket[(repeat RE)] are both equivalent to
@racket[(repeat RE 0 +inf.0)]}
@item{@racket[(+ RE)] is equivalent to @racket[(repeat RE 1 +inf.0)]}
@item{@racket[(? RE)] is equivalent to @racket[(repeat RE 0 1)]}
]}

@specsubform[#:literals (chars) (chars CharSet ...)]{

Interprets @racket[(union CharSet ...)] as a set of characters. The resulting
set of characters must be non-empty; otherwise, a syntax error is
raised. Generation of the @racket[pregexp] literal depends on only the set of
characters, not how it was originally expressed.

@specsubform[chars-string]{

Represents the set of characters appearing in the string. No character in the
string is interpreted specially. For example, @litchar{-} represents the
character @racket[#\-]; it is not interpreted as a range.

Note that a @svar[RE] @racket[_literal-string] is treated differently.
}

}

@specsubform[literal-string]{

A string @svar[RE] is treated as the concatenation (@racket[cat]) of singleton
character sets that matches exactly that string. Special characters in the
string are escaped when the @racket[pregexp] is generated. For example:
@examples[#:eval the-eval #:label #f
(px "[ab]*z?")
(regexp-match-exact? (px "[ab]*z?") "[ab]*z?")
]

Note that a @svar[CharSet] @racket[_chars-string] is treated differently.
}

@specsubform[#:literals (inject) (inject pregexp-string)]{

Injects the given @racket[pregexp-string] into the generated output. It is
treated as having lowest precedence, so it will be wrapped if it occurs
within a higher-precedence operator. For example:
@examples[#:eval the-eval #:label #f
(px (* (inject "[ab]")))
]}

@defform[(px maybe-mode part-RE ...)
         #:grammar ([maybe-mode (code:line) #:byte])]{

Converts the @svar[RE] formed by @racket[(cat part-RE ...)] into a @tech[#:doc
'(lib "scribblings/reference/reference.scrbl")]{regexp value}. If the
@racket[#:byte] keyword is used, then the literal is created with
@racket[byte-pregexp]; otherwise, it is created with @racket[pregexp].

The generation of the @racket[pregexp] literal takes precedence into account and
inserts @litchar{(?:}@tt{_}@litchar{)} wrappers as necessary. For example:
@examples[#:eval the-eval #:label #f
(px (cat "A" (or "BB" "CCC")))
(px #:byte (repeat "BB" 3))
]

@history[#:changed "0.5" @elem{Added @racket[#:byte] mode.}]}

@defform[(rx part-RE ...)
         #:grammar ([maybe-mode (code:line) #:byte])]{

Like @racket[px], but produces a @racket[regexp] literal instead. Not all
@svar[RE] features can be expressed as a @racket[regexp]-style regular
expression. For example, a @racket[repeat] @svar[RE] with custom bounds cannot
be expressed if it contains a @racket[report] sub-@svar[RE]. If such a feature
is used, a syntax error is raised. A syntax error is also raised if a character
set has a range endpoint that is a special character such as @racket[#\-] or
@racket[#\]]; it is possible to express such character sets in
@racket[regexp]-style regular expressions, but this library currently does not
support it.

@examples[#:eval the-eval #:label #f
(rx (cat "A" (or "BB" "CCC")))
(rx (repeat (or "a" "b") 2 5))
(eval:error (rx (repeat (report "a") 2 5)))
(rx (repeat (report "a") 1 +inf.0))
(rx (+ (chars alpha digit)))
]

@history[#:changed "0.5" @elem{Added @racket[#:byte] mode.}]}
}

@defform[(define-RE name maybe-mode rhs-RE)
         #:grammar ([maybe-mode (code:line) #:byte])]{

Defines @racket[name] as a name bound to a compile-time regular expression;
@racket[name] can be used in @svar[RE] forms as an abbreviation to stand for
@racket[rhs-RE].

If @racket[name] is used as an expression, it expands to @svar[rhs-RE]'s
corresponding regular-expression literal. If the @racket[#:byte] option is
present, then a @racket[byte-pregexp] literal is produced; otherwise, a
@racket[pregexp] literal is produced. The mode declaration does not affect uses
of @racket[name] within other @svar[RE] forms.

@examples[#:eval the-eval
(define-RE As (* "A"))
As
(define-RE BBs #:byte (* "BB"))
BBs
(px (or As BBs))
]

@history[#:changed "0.5" @elem{Added @racket[#:byte] mode.}]}

@(close-eval the-eval)
