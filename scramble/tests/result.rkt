;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require rackunit
         racket/match
         racket/contract
         scramble/result)

;; Test result/c contract
(define/contract r2r
  (-> (result/c symbol? (or/c string? symbol?)) (result/c string? string?))
  (lambda (r) (match r [(ok v) (ok (symbol->string v))] [_ r])))

(check-exn #rx"contract violation"
           (lambda () (r2r 123)))
(check-exn #rx"contract violation"
           (lambda () (r2r "abc")))
(check-exn #rx"contract violation"
           (lambda () (r2r (ok 123))))
(check-exn #rx"contract violation"
           (lambda () (r2r (bad 123))))
(check-exn #rx"broke its own contract" ;; range
           (lambda () (r2r (bad 'x))))

(check-equal? (r2r (ok 'abc)) (ok "abc"))
(check-equal? (r2r (bad "x")) (bad "x"))

;; Test result/c contract
(let ()
  (define/contract r2r
    (-> (result/c symbol? (or/c string? symbol?)) (result/c string? string?))
    (lambda (r) (match r [(ok v) (ok (symbol->string v))] [_ r])))

  (check-exn #rx"contract violation"
             (lambda () (r2r 123)))
  (check-exn #rx"contract violation"
             (lambda () (r2r 'abc)))
  (check-exn #rx"contract violation"
             (lambda () (r2r "abc")))
  (check-exn #rx"contract violation"
             (lambda () (r2r (ok 123))))
  (check-exn #rx"contract violation"
             (lambda () (r2r (bad 123))))
  (check-exn #rx"broke its own contract" ;; range
             (lambda () (r2r (bad 'x))))
  (check-equal? (r2r (ok 'abc)) (ok "abc"))
  (check-equal? (r2r (bad "x")) (bad "x")))

;; Test partition-results
(check-equal? (let-values ([(good bad) (partition-results (list (ok 1) (bad 2) (ok 3)))])
                (list good bad))
              (list '(1 3) '(2)))
