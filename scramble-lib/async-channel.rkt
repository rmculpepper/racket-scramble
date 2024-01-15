#lang racket/base
(require racket/match
         racket/contract)
(provide light-async-channel?
         make-light-async-channel
         (contract-out
          [light-async-channel-put
           (-> light-async-channel? any/c any)]
          [light-async-channel-get-evt
           (-> light-async-channel? evt?)]
          [light-async-channel-get
           (-> light-async-channel? any)]
          [light-async-channel-try-get
           (-> light-async-channel? any)]))

;; ============================================================

;; (Q X) = (cons (Listof X) (Listof X))
;; - simple queue, first list is in order, second is reversed
;; - read from first list, write to second, transfer when necessary

;; empty-queue : (Q X)
(define empty-queue '(() . ()))

;; queue-empty? : (Q X) -> Boolean
(define (queue-empty? q)
  (match q
    [(cons '() '()) #t]
    [_ #f]))

;; enqueue : (Q X) X -> (Q X)
(define (enqueue q v)
  (match q
    [(cons '() ws)
     ;; This case isn't necessary, but it prevents writers from
     ;; having consistently less work to do than readers.
     (define rs (reverse (cons v ws)))
     (cons rs '())]
    [(cons rs ws)
     (cons rs (cons v ws))]))

;; dequeue : (Q X) -> (values X (Q X))
(define (dequeue q)
  (match q
    [(cons (cons v rs) ws)
     (values v (cons rs ws))]
    [(cons '() (? pair? ws))
     (match-define (cons v rs) (reverse ws))
     (values v (cons rs ws))]
    [(cons '() '())
     (error 'dequeue "empty queue")]))

;; cas-dequeue : (box (Q X)) -> X
;; PRE: box's queue is not empty
(define (cas-dequeue qb)
  (define q (unbox qb))
  (define-values (v new-q) (dequeue q))
  (cond [(box-cas! qb q new-q) v]
        [else (cas-dequeue qb)]))

;; cas-enqueue : (box (Q X)) X -> Void
(define (cas-enqueue qb v)
  (define q (unbox qb))
  (define new-q (enqueue q v))
  (cond [(box-cas! qb q new-q) (void)]
        [else (cas-enqueue qb v)]))

;; ============================================================

;; Thread-safe and break-safe, but not kill-safe: If a reader thread
;; is killed in dequeue, or if a writer thread is killed after enqueue
;; but before posting to rsema, than rsema undercounts the items
;; available in the queue. That is, reads on the queue will block even
;; when the queue is nonempty.

;; LAC[X] = (light-async-channel (box Q[X]) Semaphore)
(struct light-async-channel (b rsema)
  #:property prop:evt (lambda (self) (light-async-channel-get-evt self)))

;; make-light-async-channel : -> LAC[X]
(define (make-light-async-channel)
  (light-async-channel (box empty-queue) (make-semaphore 0)))

;; light-async-channel-put : LAC[X] X -> Void
(define (light-async-channel-put lac value)
  (match-define (light-async-channel b rsema) lac)
  (define be? (break-enabled))
  (break-enabled #f)
  (cas-enqueue b value)
  (semaphore-post rsema)
  (break-enabled be?)
  (void))

;; light-async-channel-get-evt : LAC[X] -> (evtof X)
(define (light-async-channel-get-evt lac)
  (match-define (light-async-channel b rsema) lac)
  (wrap-evt rsema (lambda (_) (cas-dequeue b))))

;; light-async-channel-get : LAC[X] -> X
(define (light-async-channel-get lac)
  (get* lac #f))

;; light-async-channel-try-get : LAC[X] -> X or #f
(define (light-async-channel-try-get lac)
  (get* lac #t))

;; get* : LAC[X] Boolean -> X or #f
(define (get* lac try-get?)
  (match-define (light-async-channel b rsema) lac)
  (define be? (break-enabled))
  (break-enabled #f)
  (cond [(semaphore-try-wait? rsema)
         (begin0 (cas-dequeue b)
           (break-enabled be?))]
        [else
         (break-enabled be?)
         (cond [try-get? #f]
               [else (sync (light-async-channel-get-evt lac))])]))
