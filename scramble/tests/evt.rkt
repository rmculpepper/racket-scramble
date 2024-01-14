#lang racket/base
(require scramble/evt
         rackunit)

(test-case "empty not ready"
  (define be (make-box-evt))
  (check-equal? (sync/timeout 0.1 be)
                #f))

(test-case "ready"
  (define be (make-box-evt))
  (check-equal? (box-evt-set! be 'ok) #t)
  (check-equal? (box-evt-set! be 'already-set) #f)
  (for ([i 10])
    (check-equal? (sync/timeout 0 be) 'ok)))

(test-case "becomes ready"
  (define be (make-box-evt))
  (thread (lambda () (sleep 0.2) (box-evt-set! be 'ok)))
  (check-equal? (sync/timeout 0 be)
                #f)
  (check-equal? (sync/timeout 1 be)
                'ok))

(test-case "other thread"
  (define be (make-box-evt))
  (define ok-sema (make-semaphore 0))
  (thread (lambda ()
            (sync be)
            (semaphore-post ok-sema)))
  (box-evt-set! be 'ok)
  (check-equal? (sync/timeout 0.2 (wrap-evt ok-sema (lambda (_) 'sema)))
                'sema))
