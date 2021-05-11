#lang scribble/manual
@(require (for-syntax racket/base)
          scribble/example
          (only-in scribble/racket make-element-id-transformer)
          (for-label racket/base racket/contract
                     scramble/inject-syntax))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require scramble/inject-syntax (only-in racket/base [quote Quote]))))

@(define quote-ref @racket[quote])
@(define-syntax Quote
   (make-element-id-transformer
     (lambda (stx) #'quote-ref)))

@; ----------------------------------------
@title[#:tag "inject-syntax"]{Compile-time Code Injection}

@defmodule[scramble/inject-syntax]

@defform[(begin/inject-syntax body ...+)]{

Evaluates the @racket[body] forms at compile time. The @racket[body]s must end
in an expression that produces a syntax object; otherwise, a syntax error is
raised. The syntax object result replaces the @racket[begin/inject-syntax]
form. In other words, a @racket[begin/inject-syntax] expression is similar to an
immediate application of an anonymous macro, except that no macro scope is added
to the resulting syntax.

Any side-effects performed by the @racket[body]s occur only once, when the
@racket[begin/inject-syntax] form is compiled. This is in contrast to
@racket[begin-for-syntax], for example, whose contents are also evaluated
whenever the enclosing module is visited.

If @racket[begin/inject-syntax] is used in an expression context, the resulting
syntax object must be an expression form; otherwise, the macro expander will
raise a syntax error.

One use of @racket[begin/inject-syntax] is @as-index{conditional compilation}:
such as conditional definitions or module imports. For example, the following
code either requires a module or defines a compatibility function depending on
the current version:
@examples[#:eval the-eval #:label #f
(require (for-syntax racket/base) scramble/inject-syntax)
(eval:alts
  (begin-for-syntax
    (define (version-less-than? v) .... (version) ....))
  (begin-for-syntax
    (define (version-less-than? v) #f)))  
(begin/inject-syntax
  (if (version-less-than? "7.6") (code:comment "racket/symbol was added in 7.6")
      #'(begin (define (symbol->immutable-string s)
                 (string->immutable-string (symbol->string s))))
      #'(begin (require (only-in racket/symbol symbol->immutable-string)))))
(symbol->immutable-string 'hello)
]

The following example checks whether an identifier is currently defined (useful,
for example, when a new name is added to an existing module):
defined:
@examples[#:eval the-eval #:label #f
(require racket/string)
(begin/inject-syntax
  (if (identifier-binding #'string-exclaim) (code:comment "Already defined? (No)")
      #'(begin)
      #'(define (string-exclaim str)
          (regexp-replace* #rx"[.]" str "!"))))
(string-exclaim "Thanks. Have a nice day.")
]

The following example selects between different implementations (eg, safe vs
unsafe or with contracts vs without contracts) based on a compile-time
configuration variable:
@examples[#:eval the-eval #:label #f
(require racket/require)
(define-for-syntax use-safe-fx-ops? #t)
(begin/inject-syntax
  (if use-safe-fx-ops?
      #'(require (prefix-in unsafe- racket/fixnum))
      #'(require (matching-identifiers-in #rx"^unsafe-fx" racket/unsafe/ops))))
(unsafe-fx+ 1 2)
]

Keep in mind that it is customary in Racket to do library configuration at run
time via ordinary variables, parameters, etc. Prefer run-time mechanisms when
possible. Use @racket[begin/inject-syntax] when compile-time concerns are
involved, such as scoping and variations in module exports.

Another use is to perform compile-time computations of quotable data:
@examples[#:eval the-eval #:label #f #:escape UNSYNTAX
(begin-for-syntax
  (define (slow-popcount n)
    (cond [(zero? n) 0]
          [else (+ (slow-popcount (quotient n 2))
                   (if (bitwise-bit-set? n 0) 1 0))])))
(define (faster-popcount n)
  (define (byte-popcount b)
    (bytes-ref (begin/inject-syntax
                 #`(Quote #,(apply bytes (for/list ([n 256]) (slow-popcount n)))))
               b))
  (cond [(zero? n) 0]
        [else (+ (faster-popcount (arithmetic-shift n -8))
                 (byte-popcount (bitwise-bit-field n 0 8)))]))
(eval:alts
  (faster-popcount (UNSYNTAX @(racketvalfont "#xFFFFFFFF")))
  (faster-popcount #xFFFFFFFF))
]

@bold{Warning:} Code can be run on a different platform from the one it was
compiled on. Don't use compile-time conditions to specialize code based on
features that may change between compile time and run time. On Racket BC, these
include the size of fixnums, the operating system, path conventions, and so on.
On Racket CS, these features might not change, but beware of cross-compilation.
}


@; ------------------------------------------------------------
@(close-eval the-eval)
