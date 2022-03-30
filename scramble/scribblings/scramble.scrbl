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
                     scramble/evt
                     scramble/struct
                     ))

@title{scramble: Assorted Utility Libraries}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@; ----------------------------------------

@include-section["about.scrbl"]
@include-section["cond.scrbl"]
@include-section["evt.scrbl"]
@include-section["function.scrbl"]
@include-section["immutable.scrbl"]
@include-section["list.scrbl"]
@include-section["number.scrbl"]
@include-section["result.scrbl"]
@include-section["slice.scrbl"]
@include-section["struct.scrbl"]

@; ----------------------------------------

@include-section["class.scrbl"]
@include-section["inject-syntax.scrbl"]
@include-section["struct-info.scrbl"]
