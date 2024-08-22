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

;; ============================================================
;; Mutex

;; Thread-safe and break-safe, but not kill-safe.
;; See marked points below where invariant temporarily broken.
;; The mutex state is determined by contents of b, not sema.

;; Mutex[X] = (mutex (box (U #f (cons ThreadDeadEvt X))) Semaphore)
;; Invariant:
;;   b contains #f     iff  sema contains 1    = UNLOCKED state
;;   b contains owner  iff  sema contains 0    = LOCKED state
(struct mutex (b sema)
  #:property prop:evt
  (lambda (self) (mutex-acquire-evt self)))

(define (make-mutex)
  (mutex (box #f) (make-semaphore 1)))

(define (mutex-acquire mx [data #f]
                       #:on-hopeless [hopeless-handler #f]
                       #:enable-break? [enable-break? #f])
  (match-define (mutex b sema) mx)
  (define me (cons (thread-dead-evt (current-thread)) data))
  (define be? (break-enabled))
  (parameterize-break #f
    (let retry-loop ()
      (cond [(box-cas! b #f me)
             ;; <-- Invariant does not hold here
             (semaphore-try-wait? sema) ;; must not block
             (void)]
            [else
             (match (unbox b)
               [#f
                (retry-loop)]
               [(cons owner-evt owner-data)
                ((if (or enable-break? be?) sync/enable-break sync)
                 (handle-evt owner-evt
                             (lambda (_e)
                               (cond [hopeless-handler (hopeless-handler owner-data)]
                                     [else (error:hopeless owner-data)])))
                 (handle-evt (semaphore-peek-evt sema)
                             (lambda (_e) (retry-loop))))])]))))

(define (error:hopeless owner-data)
  (error 'mutex-acquire "mutex is locked by a dead thread\n  owner data: ~e"
         owner-data))

(define (mutex-release mx)
  (match-define (mutex b sema) mx)
  (parameterize-break #f
    (let retry-loop ()
      (define owner+data (unbox b))
      (unless owner+data (error:unlocked))
      (cond [(box-cas! b owner+data owner+data #f)
             ;; <-- Invariant does not hold here
             (semaphore-post sema)
             (void)]
            [else
             (retry-loop)]))))

(define (error:unlocked)
  (error 'mutex-release "mutex is already unlocked"))
