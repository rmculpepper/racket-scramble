#lang racket/base
(require racket/match
         racket/contract
         scramble/bundle
         rackunit)

;; ============================================================

(define-interface equal+hash$
  (equal-to?    ;; X X (X X -> Boolean) Boolean -> Boolean
   hashcode     ;; X (X -> Integer) Boolean -> Boolean
   ;; final arg = whether to consider mutable data's current value
   )
  #:derive-property prop:equal+hash
  (list (lambda (self other recur mut-mode?)
          ($equal-to? self other recur mut-mode?))
        (lambda (self recur mut-mode?)
          ($hashcode self recur mut-mode?)))
  #:generics-prefix $)

(define-interface custom-write$
  (custom-write ;; X OutputPort Mode -> Void
   )
  #:derive-property prop:custom-write
  (lambda (self out mode) ($custom-write self out mode))
  #:generics-prefix $)

;; ----------------------------------------

(define-interface S (push peek pop mt mt?))
(define-interface Q (enq deq mtq))

(define S@
  (bundle
   #:export (S)
   (define push cons)
   (define peek car)
   (define pop cdr)
   (define mt null)
   (define mt? null?)))

(define Q@
  (bundle
   #:export (Q)
   #:import (S)
   (define (rev s [acc mt])
     (if (mt? s) acc (rev (pop s) (push (peek s) acc))))
   ;; ----
   (define mtq (vector mt mt))
   (define (enq q v)
     (match-define (vector rs ws) q)
     (vector rs (push v ws)))
   (define (deq q)
     (match-define (vector rs ws) q)
     (cond [(mt? rs) (deq (vector (rev ws) mt))]
           [else (values (peek rs) (vector (pop rs) ws))]))))

(define U@
  (bundle
   #:import (Q)
   (define q (enq (enq (enq mtq 1) 2) 3))
   (println (call-with-values (lambda () (deq q)) list))))

(let ()
  (define/invoke-bundles #:export (Q) S@ Q@)
  (check-equal? mtq (vector null null)))

;; ----------------------------------------

(define-interface L
  ([len #:dynamic-public]
   [lenscale (-> any/c real? real?)])
  #:fallbacks
  (hash 'lenscale
        (lambda (self x) (let ([d (len self)]) (* x d d)))))

(struct point (x y)
  #:properties
  (method-properties
   #:export ([L #:prefix %])
   #:import ([L #:tag (super) #:prefix super-])
   (define (%len self)
     ;; (set! %blah void) ;; should cause syntax error
     (match self [(point x y) (sqrt (+ (* x x) (* y y)))]))
   (define (%lenscale self v)
     (printf "super: ~e\n" (super-lenscale self v))
     'no
     #;0)))

(define p (point 3 4))

(check-equal? (len p) 5)
(check-exn #rx"broke its own contract"
           (lambda ()
             (parameterize ((current-output-port (open-output-bytes)))
               (lenscale p 1))))

(define-interface H (helper))

(define helper%
  (bundle
   #:export (H)
   #:import ([L #:tag (super) #:prefix super-])
   (define (helper self)
     (printf "helper!\n")
     (printf "super: ~e\n" (super-lenscale self 1))
     100)))

(struct point2 (x y)
  #:properties
  (method-properties
   #:export ([L #:prefix %])
   #:import (H)
   #:link (list helper%)
   (define (%len self)
     ;; (set! %blah void) ;; should cause syntax error
     (match self [(point2 x y) (sqrt (+ (* x x) (* y y)))]))
   (define (%lenscale self v)
     (printf "point2!\n")
     (helper self))))

(define p2 (point2 5 12))

(check-equal? (len p2) 13)
(check-equal? (parameterize ((current-output-port (open-output-bytes)))
                (lenscale p2 1))
              100)
