#lang racket/base

(define (raise-read-error+ #:eof? [eof? #f]
                           #:extra-srclocs [extra-srclocs null]
                           loc fmt . args)
  (define msg (apply format fmt args))
  (define prefixed-msg
    (cond [(and (error-print-source-location) (srcloc->string loc))
           => (lambda (prefix) (string-append prefix ": " msg))]
          [else msg]))
  (define cms (current-continuation-marks))
  (define srclocs (cons loc extra-srclocs))
  (cond [eof? (raise (exn:fail:read:eof prefixed-msg cms srclocs))]
        [else (raise (exn:fail:read prefixed-msg cms srclocs))]))

;; prop:exn/info
;; exn-info? : Any -> Boolean
;; exn-info : Exn -> Any
