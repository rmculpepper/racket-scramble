;; Copyright 2021-2024 Ryan Culpepper
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

;; Thread-safe and break-safe, but not kill-safe: If a thread is killed in
;; box-evt-set! after successful CAS but before posting to rsema, then threads
;; already sync'd on box-evt are not woken, but new threads can sync on box-evt.

(define unset (string->uninterned-symbol "<UNSET>"))

;; BoxEvt[X] = (box-evt (box (U X unset)) Semaphore Semaphore Evt[X])
(struct box-evt (b rsema)
  #:property prop:evt
  (lambda (self)
    (match-define (box-evt b rsema) self)
    (wrap-evt (cond [(eq? (unbox b) unset)
                     (semaphore-peek-evt rsema)]
                    [else always-evt])
              (lambda (_e) (unbox b)))))

;; make-box-evt : -> BoxEvt[X]
(define (make-box-evt)
  (box-evt (box unset) (make-semaphore 0)))

;; box-evt-set! : BoxEvt[X] X -> Boolean
;; Returns #t if we set, #f if already set.
(define (box-evt-set! be v)
  (match-define (box-evt b rsema) be)
  (define be? (break-enabled))
  (break-enabled #f)
  (cond [(box-cas! b unset v)
         (semaphore-post rsema)
         (break-enabled be?)
         #t]
        [else
         (break-enabled be?)
         ;; CAS failed, but real or spurious failure?
         (cond [(eq? (unbox b) unset)
                ;; spurious failure, retry
                (box-evt-set! be v)]
               [else
                ;; real failure
                #f])]))

;; box-evt-ready? : BoxEvt[X] -> Boolean
(define (box-evt-ready? be)
  (define b (box-evt-b be))
  (not (eq? (unbox b) unset)))

#;
(module* atomic-box-evt #f
  (require ffi/unsafe/atomic)
  (provide (contract-out
            [atomic-box-evt-set!
             (-> box-evt? any/c boolean?)]))

  ;; atomic-box-evt-set! : BoxEvt[X] X -> Boolean
  ;; Like box-evt-set! but kill-safe.
  (define (atomic-box-evt-set! be v)
    (match-define (box-evt b rsema) be)
    (start-atomic)
    (cond [(eq? (unbox b) unset)
           (set-box! b v)
           (semaphore-post rsema)
           (end-atomic)
           #t]
          [else
           (end-atomic)
           #f])))
