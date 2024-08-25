#lang racket/base
(require racket/sequence
         scramble/relation
         rackunit)

(define rel-with-lookups
  (relation
   #:heading
   ['x 'y]
   #:tuples
   ['a 1]
   ['b 1]
   ['c 2]
   #:unique 'x
   #:lookup 'y))

(define rel-no-lookups
  (relation
   #:heading
   ['x 'y]
   #:tuples
   ['a 1]
   ['b 1]
   ['c 2]))

(define dynamic-rel
  (make-relation 'dynamic '(x y) '((a 1) (b 1) (c 2))))

(define (test-rel rel)
  (check-equal? (relation-heading rel) '#(x y))
  (check-equal? (relation-tuples rel) '#(#(a 1) #(b 1) #(c 2)))
  (check-equal? (relation-index rel 'x) 0)
  (check-equal? (relation-index rel 'y) 1)
  (check-equal? (sequence->list (in-relation rel 'x)) '(a b c))
  (check-equal? (sequence->list (in-relation rel '(x y))) '((a 1) (b 1) (c 2)))
  (check-equal? (relation-find rel 'x 'a #f) '#(a 1))
  (check-equal? (relation-find rel 'x 'z #f) #f)
  (check-equal? (relation-find rel 'y 1 #f) '#(a 1)) ;; order!
  (check-equal? (relation-find rel 'y 2 #f) '#(c 2))
  (check-equal? (relation-find rel 'x 'a #t) '(#(a 1)))
  (check-equal? (relation-find rel 'y 1 #t) '(#(a 1) #(b 1)))
  (check-equal? (relation-ref rel 'x 'a 'y) 1)
  (check-equal? (relation-ref rel 'x 'z 'y) #f)
  (check-equal? (relation-ref rel 'x 'z 'y 0) 0)
  (check-equal? (relation-ref rel 'y 1 'x) 'a) ;; order!
  (check-equal? (relation-ref rel 'y 2 'x) 'c)
  (check-equal? (relation-ref rel 'y 9 'x) #f)
  (check-equal? (relation-ref rel 'x 'a '(x y)) '(a 1))
  (check-equal? (relation-ref-all rel 'x 'a 'y) '(1))
  (check-equal? (relation-ref-all rel 'y 1 'x) '(a b)) ;; order
  (check-equal? (relation-ref-all rel 'x 'z 'y) null)
  (void))

(test-rel rel-with-lookups)
(test-rel rel-no-lookups)
(test-rel dynamic-rel)
