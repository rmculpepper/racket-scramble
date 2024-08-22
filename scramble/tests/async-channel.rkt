#lang racket/base
(require racket/match
         scramble/async-channel
         rackunit)

(test-case "basic"
  (define lac (make-light-async-channel))

  (check-equal? (sync/timeout 0 lac)
                #f)
  (check-equal? (sync/timeout 0 (light-async-channel-get-evt lac))
                #f)

  (for ([i 2])
    (light-async-channel-put lac i))

  (check-equal? (sync/timeout 0 lac)
                0)
  (check-equal? (light-async-channel-get lac)
                1)
  (check-equal? (sync/timeout 0 (light-async-channel-get-evt lac))
                #f)
  (void))

(test-case "wait"
  (define lac (make-light-async-channel))
  (thread (lambda ()
            (sleep 0.2)
            (for ([i 3]) (light-async-channel-put lac 'ok))))
  (check-equal? (sync/timeout 1 lac)
                'ok)
  (check-equal? (light-async-channel-get lac)
                'ok))

(test-case "workers"
  (define work-lac (make-light-async-channel))
  (define sum-lac (make-light-async-channel))
  (define WORKERS 10)
  (define N #e1e5)

  (for ([worker WORKERS])
    (thread (lambda ()
              (let loop ([sum 0])
                (match (sync work-lac)
                  [(? real? n)
                   (loop (+ sum n))]
                  ['stop
                   (light-async-channel-put sum-lac sum)])))))

  (for ([i N])
    (when (zero? (remainder i (quotient N 19)))
      (sleep 0.13))
    (light-async-channel-put work-lac i))
  (for ([i WORKERS])
    (light-async-channel-put work-lac 'stop))

  (define total
    (for/sum ([i WORKERS])
      (sync sum-lac)))
  (check-equal? total
                (for/sum ([i N]) i)))

(test-case "limit"
  (define lac (make-light-async-channel 2))

  (light-async-channel-put lac 1)
  (light-async-channel-put lac 2)
  (check-equal? (sync/timeout 0 (light-async-channel-put-evt lac 3))
                #f)

  (check-equal? (sync lac) 1)
  (check-equal? (sync/timeout 0 (light-async-channel-put-evt lac 4))
                (void))

  (void))
