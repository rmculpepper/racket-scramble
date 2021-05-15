#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract racket/class
                     scramble/class scramble/about))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require racket/class scramble/class)))


@; ----------------------------------------
@title[#:tag "class"]{Classes and Objects}

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
@racket['never].

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
@(close-eval the-eval)
