#lang racket/base
(require racket/match
         racket/contract)
(provide light-async-channel?
         (contract-out
          [make-light-async-channel
           (->* [] [(or/c exact-nonnegative-integer? #f)] any)]
          [light-async-channel-put-evt
           (-> light-async-channel? any/c any)]
          [light-async-channel-put
           (-> light-async-channel? any/c any)]
          [light-async-channel-get-evt
           (-> light-async-channel? evt?)]
          [light-async-channel-get
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
     (values v (cons rs '()))]
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

;; Thread-safe and break-safe, but not kill-safe:
;; - If a reader thread is killed in dequeue, or if a writer thread is killed
;;   after enqueue but before posting to rsema, than rsema undercounts the items
;;   available in the queue. That is, reads on the queue will block even when
;;   the queue is nonempty.
;; - If the queue has a limit (wsema is not false), then if a writer thread is
;;   killed before enqueue, or if a reader thread is killed after dequeue but
;;   before posting to wsema, then wsema undercounds the available capacity. That
;;   is, writes to the queue will block even when the queue is under the limit.

;; LAC[X] = (light-async-channel (box Q[X]) Semaphore Semaphore/#f)
;; Invariants:
;; - ReadInv: rsema's counter = number of elements in queue
;; - WriteInv: wsema's counter = remaining capacity (limit - number of elements)
;; - WeakInv: rsema's counter <= number of elements in queue
;;            wsema's counter <= remaining capacity (limit - number of elements)
(struct light-async-channel (b rsema wsema)
  #:property prop:evt (lambda (self) (light-async-channel-get-evt self)))

;; make-light-async-channel : -> LAC[X]
(define (make-light-async-channel [limit #f])
  (light-async-channel (box empty-queue)
                       (make-semaphore 0)
                       (and limit (make-semaphore limit))))

;; light-async-channel-put-evt : LAC[X] X -> Void
(define (light-async-channel-put-evt lac value)
  (match-define (light-async-channel b rsema wsema) lac)
  (wrap-evt (or wsema always-evt)
            (lambda (_)
              ;; <-- WriteInv does not hold here (if wsema present)
              (cas-enqueue b value)
              ;; <-- ReadInv does not hold here
              (semaphore-post rsema))))

;; light-async-channel-put : LAC[X] X -> Void
(define (light-async-channel-put lac value)
  (match-define (light-async-channel b rsema wsema) lac)
  (define be? (break-enabled))
  (parameterize-break #f
    (when wsema
      (if be? (semaphore-wait/enable-break wsema) (semaphore-wait wsema)))
    ;; <-- WriteInv does not hold here (if wsema present)
    (cas-enqueue b value)
    ;; <-- ReadInv does not hold here
    (semaphore-post rsema)
    (void)))

;; light-async-channel-get-evt : LAC[X] -> (evtof X)
(define (light-async-channel-get-evt lac)
  (match-define (light-async-channel b rsema wsema) lac)
  (wrap-evt rsema
            (lambda (_)
              ;; <-- ReadInv does not hold here
              (begin0 (cas-dequeue b)
                ;; <-- WriteInv does not hold here (if wsema present)
                (when wsema (semaphore-post wsema))))))

;; light-async-channel-get : LAC[X] -> X
(define (light-async-channel-get lac)
  (match-define (light-async-channel b rsema wsema) lac)
  (define be? (break-enabled))
  (parameterize-break #f
    (if be? (semaphore-wait/enable-break sema) (semaphore-wait rsema))
    ;; <-- ReadInv does not hold here
    (begin0 (cas-dequeue b)
      ;; <-- WriteInv does not hold here (if wsema present)
      (when wsema (semaphore-post wsema)))))
