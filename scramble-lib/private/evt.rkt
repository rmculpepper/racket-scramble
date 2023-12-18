#lang racket/base
(require racket/match
         ffi/unsafe/atomic)

;; ============================================================

;; Thread-safe and break-safe, but not kill-safe: If a reader thread
;; is killed in dequeue, or if a writer thread is killed after enqueue
;; but before posting to rsema, than rsema undercounts the items
;; available in the queue. That is, reads on the queue will block even
;; when the queue is nonempty.

;; LWAC[X] = (lwac (box (cons (Listof X) (Listof X))) Semaphore)
(struct lwac (b rsema)
  #:property prop:evt
  (lambda (self)
    (match-define (lwac b rsema) self)
    (wrap-evt rsema (lambda (_) (dequeue b)))))

(define (make-lwac)
  (lwac (box (cons null null)) (make-semaphore 0)))

(define (dequeue b)
  (define q (unbox b))
  (define-values (v new-q)
    (match q
      [(cons '() rxs)
       (define xs (reverse rxs))
       (values (car xs) (cons xs null))]
      [(cons (cons x xs) rxs)
       (values x (cons xs rxs))]))
  (if (box-cas! b q new-q)
      v
      (dequeue b)))

(define (lwac-put! lw v)
  (match-define (lwac b rsema) lw)
  (define be? (break-enabled))
  (break-enabled #f)
  (enqueue b v)
  (semaphore-post rsema)
  (break-enabled be?))

(define (enqueue b v)
  (define q (unbox b))
  (define new-q
    (match q
      [(cons '() rxs)
       ;; This case isn't necessary, but it prevents writers from
       ;; having consistently less work to do than readers.
       (define xs (reverse rxs))
       (cons (cons v xs) null)]
      [(cons xs rxs)
       (cons xs (cons v rxs))]))
  (if (box-cas! b q new-q)
      (void)
      (enqueue b v)))
