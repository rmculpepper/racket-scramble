#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract scramble/async-channel))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require scramble/async-channel)))


@; ----------------------------------------
@title[#:tag "async-channel"]{Async-Channels}

@defmodule[scramble/async-channel]

@defproc[(make-light-async-channel) box-evt?]{

Returns a new @deftech{box event}. A box event becomes ready for synchronization
when it is filled with a value by @racket[box-evt-set!], and its synchronization
result is the value in the box. Once a box event becomes ready, it remains
ready, and its contents cannot be changed.

Box events are thread-safe and break-safe but not kill-safe: If a thread is
killed during a call to @racket[box-evt-set!], it is possible for the box event
to become damaged---that is, unready for synchronization but also unable to be
filled by another call to @racket[box-evt-set!].

@examples[#:eval the-eval
(define bxe (make-box-evt))
(sync/timeout 0 bxe)
(box-evt-set! bxe (list 1 2 3))
(sync/timeout 0 bxe)
(box-evt-set! bxe (list 7 8 9))
(sync/timeout 0 bxe)
]}

@defproc[(box-evt? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{box event} produced by
@racket[make-box-evt], @racket[#f] otherwise.
}

@defproc[(box-evt-set! [bxe box-evt?] [v any/c]) boolean?]{

If @racket[bxe] is not ready, then fills it with @racket[v], causing
@racket[bxe] to become ready, and returns @racket[#t]. If @racket[bxe] is
already ready, returns @racket[#f] without changing @racket[bxe]'s contents.
}

@defproc[(box-evt-ready? [bxe box-evt?]) boolean?]{

Returns @racket[#t] if @racket[bxe] is ready for synchronization, @racket[#f]
otherwise.

Equivalent to @racket[(sync/timeout 0 (wrap-evt bxe (lambda (v) #t)))].
}


@; ----------------------------------------
@(close-eval the-eval)
