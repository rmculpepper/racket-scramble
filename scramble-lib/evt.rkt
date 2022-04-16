;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         racket/contract/base)
(provide box-evt?
         make-box-evt
         (contract-out
          [box-evt-set!
           (-> box-evt? any/c boolean?)]
          [box-evt-ready?
           (-> box-evt? boolean?)]))

;; ============================================================

;; Thread-safe and break-safe, but not kill-safe: If a thread is
;; killed in box-evt-set! between acquiring wsema and posting to
;; rsema, then the box-evt can never become ready.

;; BoxEvt[X] = (box-evt (box (U X #f)) Semaphore Semaphore Evt[X])
(struct box-evt (b wsema rsema evt)
  #:property prop:evt (struct-field-index evt))

(define (make-box-evt)
  (define b (box #f))
  (define wsema (make-semaphore 1))
  (define rsema (make-semaphore 0))
  (define evt ;; FIXME: wrap or handle?
    (wrap-evt (semaphore-peek-evt rsema)
              (lambda (_e) (unbox b))))
  (box-evt b wsema rsema evt))

;; box-evt-set! : BoxEvt X -> Boolean
;; Returns #t if we set, #f if already set.
;; Thread-safe and break-safe but not kill-safe.
(define (box-evt-set! be v)
  (match-define (box-evt b wsema rsema evt) be)
  (define be? (break-enabled))
  (break-enabled #f)
  (cond [(semaphore-try-wait? wsema)
         ;; Consumed wsema; don't post to wsema when done.
         (set-box! b v)
         (semaphore-post rsema)
         (break-enabled be?)
         #t]
        [else
         (break-enabled be?)
         #f]))

(define (box-evt-ready? be)
  (and (sync/timeout 0 (semaphore-peek-evt (box-evt-rsema be))) #t))

#;
(module* atomic-box-evt #f
  (require ffi/unsafe/atomic)
  (provide (contract-out
            [atomic-box-evt-set!
             (-> box-evt? any/c boolean?)]))

  ;; atomic-box-evt-set! : BoxEvt[X] X -> Boolean
  ;; Like box-evt-set! but kill-safe.
  (define (atomic-box-evt-set! be v)
    (match-define (box-evt b wsema rsema evt) be)
    (start-atomic)
    (cond [(semaphore-try-wait? wsema)
           ;; Consumed wsema; don't post to wsema when done.
           (set-box! b v)
           (semaphore-post rsema)
           (end-atomic)
           #t]
          [else
           (end-atomic)
           #f])))
