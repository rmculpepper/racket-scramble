#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract racket/sequence
                     scramble/contract scramble/relation))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require racket/sequence scramble/relation)))

@; ----------------------------------------
@title[#:tag "relation"]{Relations}

The relation structure itself is immutable, but they may contains mutable field
values. Mutating a field value may cause the relation's lookups to behave
incorrectly.

The relations perform all field comparisons using @racket[equal?].

@defmodule[scramble/relation]

@history[#:added "0.4"]

@defproc[(relation? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a relation created by @racket[relation] or
@racket[make-relation]; otherwise, returns @racket[#f].
}

@deftogether[[
@defproc[(relation-heading [rel relation?]) (vectorof symbol?)]
@defproc[(relation-tuples [rel relation?]) (vectorof vector?)]
]]{

Returns the relations's heading and tuples, respectively. The heading is a
vector of field names, and each tuple in the relation is a vector of the same
length.
}

@defform[(relation maybe-name heading tuples lookup ...)
         #:grammar
         ([maybe-name (code:line)
                      (code:line #:name name-expr)]
          [heading (code:line #:heading [field-name-expr ...])]
          [tuples (code:line #:tuples [field-value-expr ...] ...)]
          [lookup (code:line #:lookup field-name-expr)
                  (code:line #:unique field-name-expr)])
         #:contracts
         ([field-name-expr symbol?])]{

Creates a relation.  The result of @racket[name-expr], if present, is only used
by the printer to aid debugging. The @racket[heading] defines the symbolic names
of the relation's fields. Each tuple consists of the same number of field value
expressions, where each field value's position corresponds to the field's
position in the heading.

@margin-note{A ``lookup'' would be called an ``index'' in database terminology,
but this library uses the term ``lookup'' to avoid ambiguity with ``index'' as a
numeric position.}  Each @racket[lookup] declaration causes the relation to
include a hash mapping field values to tuples. If the lookup is declared
@racket[#:unique], then the relation must contain at most one tuple for a given
field value.

@examples[#:eval the-eval
(define food
  (relation #:name 'food
            #:heading
            ['food 'type 'color]
            #:tuples
            ['apple 'fruit 'red]
            ['banana 'fruit 'yellow]
            ['shishkebab 'meat 'brown]
            ['artichoke 'vegetable 'green]
            ['broccoli 'vegetable 'green]
            #:unique 'food))
food
]}

@defproc[(make-relation [name (or/c #f symbol?)]
                        [heading (vectorof symbol?)]
                        [tuples (vectorof vector?)])
         relation?]{

Creates a relation using the given @racket[name], @racket[heading], and
@racket[tuples]. Lookups must be added separately using
@racket[relation-add-lookup].
}

@defproc[(relation-add-lookup [rel relation?]
                              [field symbol?]
                              [unique? boolean?])
         relation?]{

Produces a relation like @racket[rel] but with a lookup for
@racket[field]. If @racket[unique?] is true, then each field value in
@racket[rel] for @racket[field] must be distinct; otherwise, an exception
is raised.
}

@defproc[(relation-index [rel relation?] [field symbol?])
         (or/c #f exact-nonnegative-integer?)]{

Returns the position of @racket[field] in the relation @racket[rel]'s
heading, or @racket[#f] if @racket[rel]'s heading does not contain
@racket[field].

@examples[#:eval the-eval
(relation-index food 'food)
(relation-index food 'color)
(relation-index food 'calories)
]}

@defproc[(in-relation [rel relation?]
                      [field/s (or/c symbol? (listof symbol?))])
         sequence?]{

Returns a sequence that produces one element for each tuple in @racket[rel]. If
@racket[field/s] is a single symbol, then each element is the tuple's value for
that field. If @racket[field/s] is a list of symbols, then each element is a
list of the corresponding field values.

@examples[#:eval the-eval
(sequence->list (in-relation food 'type))
(sequence->list (in-relation food '(food type)))
]}

@defproc[(relation-find [rel relation?]
                        [key-field symbol?]
                        [key-value any/c]
                        [all? boolean? #f])
         (if/c all? (listof vector?) (or/c vector? #f))]{

Find tuples whose value for @racket[key-field] is equal to
@racket[key-value]. If @racket[all?] is true, then a list of all matching tuples
is returned. If @racket[all?] is false, then only the first tuple is returned,
or @racket[#f] if no tuple matches.

@examples[#:eval the-eval
(relation-find food 'type 'fruit)
(relation-find food 'type 'vegetable #t)
]}

@defproc[(relation-ref [rel relation?]
                       [key-field symbol?]
                       [key-value any/c]
                       [result-field/s (or/c symbol? (listof symbol?))]
                       [default any/c #f])
         any]{

Returns the @racket[result-field/s] for the first tuple in @racket[rel] whose
value for @racket[key-field] is equal to @racket[key-value].

If @racket[result-field/s] is a single symbol, then the corresponding field
value is returned. If @racket[result-field/s] is a list of symbols, then a list
of the corresponding field values is returned.

If @racket[rel] contains no matching tuple, @racket[default] is called if it is
a procedure or returned otherwise. Note that unlike @racket[hash-ref], the
default value of @racket[dfault] is @racket[#f], not an error-raising procedure.

@examples[#:eval the-eval
(relation-ref food 'food 'apple 'type)
(relation-ref food 'food 'brie 'type)
(relation-ref food 'food 'artichoke '(type color))
]}

@defproc[(relation-ref-all [rel relation?]
                           [key-field symbol?]
                           [key-value any/c]
                           [result-field/s (or/c symbool? (listof symbol?))])
         list?]{

Like @racket[relation-ref], but returns a list of results based on all matching
tuples.

@examples[#:eval the-eval
(relation-ref-all food 'type 'fruit '(food color))
]}

@deftogether[[
@defproc[(relation-prepare-find [rel relation?]
                                [key-field symbol?]
                                [all? boolean? #f])
         (-> any/c
             (if/c all? (listof vector?) (or/c #f vector?)))]
@defproc[(relation-prepare-ref [rel relation?]
                               [key-field symbol?]
                               [result-field/s (or/c symbol? (listof symbol?))])
         (->* [any/c] [any/c] any)]
@defproc[(relation-prepare-ref-all [rel relation?]
                                   [key-field symbol?]
                                   [result-field/s (or/c symbol? (listof symbol?))])
         (-> any/c list?)]
]]{

Like @racket[relation-find], @racket[relation-ref], and
@racket[relation-ref-all], except that the field value is not supplied
immediately, and the result is a procedure. Calling the prepared procedure with
the field value produces the relation's result.

@examples[#:eval the-eval
(define food-color (relation-prepare-ref food 'food 'color))
(food-color 'banana)
(food-color 'artichoke)
(food-color 'haggis 'unknown)
]}
