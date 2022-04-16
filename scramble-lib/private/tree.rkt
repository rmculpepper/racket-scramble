;; Copyright 2020 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(provide (all-defined-out))

;; F[X] = (list X ...)
;;      | (list* X ... X)
;;      | (vector X ...)
;;      | (make-prefab-struct Key X ...)
;;      | (hash{,eq,eqv} { Key X } ...)
;;      | (box X)
;;      | Any

;; T = F[T]

;; child-map : F[X] (X -> X) -> F[X]
;; Note: continuation-safe. Result F parts are not impersonated.
(define (child-map x f)
  (cond [(pair? x)
         (let pairloop ([x x])
           (if (pair? x)
               (let ([h (f (car x))] [t (pairloop (cdr x))])
                 (if (and (eq? h (car x)) (eq? t (cdr x))) x (cons h t)))
               (if (null? x) null (f x))))]
        [(vector? x)
         (define fes (for/list ([e (in-vector x)]) (f e)))
         (cond [(and (immutable? x) (not (impersonator? x))
                     (for/and ([e (in-vector x)] [fe (in-list fes)]) (eq? e fe)))
                x]
               [(immutable? x) (vector->immutable-vector (list->vector fes))]
               [else (list->vector fes)])]
        [(prefab-struct-key x)
         => (lambda (key)
              (define xv (struct->vector x))
              (define fes (for/list ([e (in-vector xv 1)]) (f e)))
              (cond [(and (not (impersonator? x)) (immutable-prefab-key? key)
                          (for/and ([e (in-vector xv 1)] [fe (in-list fes)]) (eq? e fe)))
                     x]
                    [else (apply make-prefab-struct key fes)]))]
        [(hash? x)
         (if (immutable? x)
             (if (impersonator? x)
                 (for/fold ([h (hash-copy-clear x)]) ([(k v) (in-hash x)])
                   (hash-set h k (f v)))
                 (for/fold ([h x]) ([(k v) (in-hash x)])
                   (define fv (f v))
                   (if (eq? v fv) h (hash-set h k fv))))
             (let ([kvs (for/fold ([acc null]) ([(k v) (in-hash x)])
                          (cons (cons k (f v)) acc))])
               (define h (hash-copy-clear x))
               (for ([kv (in-list kvs)]) (hash-set! h (car kv) (cdr kv)))
               (h)))]
        [(box? x)
         (let ([fv (f (unbox x))])
           (cond [(and (not (impersonator? x)) (eq? fv (unbox x))) x]
                 [else (if (immutable? x) (box-immutable fv) (box fv))]))]
        [else x]))

(define (immutable-prefab-key? key)
  (not (and (list? key) (for/or ([c (in-list key)]) (or (pair? c) (vector? c))))))

;; tree-transform : T (T -> T) Boolean -> T
(define (tree-transform x post-f [root? #t])
  (define (loop x) (post-f (loop* x)))
  (define (loop* x) (child-map x loop))
  (if root? (loop x) (loop* x)))

;; tree-transform-preorder : T (T ([T] -> T) -> T) Boolean -> T
(define (tree-transform-preorder x pre-f [root? #t])
  (define (loop x) (pre-f x (lambda ([y x]) (loop* y))))
  (define (loop* x) (child-map x loop))
  (if root? (loop x) (loop* x)))

;; ------------------------------------------------------------

;; child-reduce : F[X] (X -> Y) (Y ... -> Y) -> Y
(define (child-reduce x f reduce)
  (cond [(pair? x)
         (define xs
           (let pairloop ([x x])
             (if (pair? x)
                 (cons (f (car x)) (pairloop (cdr x)))
                 (if (null? x) null (list (f x))))))
         (apply reduce xs)]
        [(vector? x)
         (apply reduce (for/list ([e (in-vector x)]) (f e)))]
        [(prefab-struct-key x)
         (apply reduce (for/list ([e (in-vector (struct->vector x) 1)]) (f e)))]
        [(hash? x)
         (apply reduce (for/list ([v (in-hash-values x)]) (f v)))]
        [(box? x)
         (f (unbox x))]
        [else (reduce)]))

;; child-reduce-left : F[X] (X -> Y) (and (-> Y) (Y Y -> Y)) -> Y
;; Avoids apply and aux lists, but bad for eg append.
(define (child-reduce-left x f reduce)
  (cond [(pair? x) ;; LEFT
         (let pairloop ([acc (reduce)] [x x])
           (if (pair? x)
               (pairloop (reduce acc (f (car x))) (cdr x))
               (if (null? x) acc (reduce acc (f x)))))]
        [(vector? x)
         (for/fold ([acc (reduce)]) ([e (in-vector x)])
           (reduce acc (f e)))]
        [(prefab-struct-key x)
         (for/fold ([acc (reduce)]) ([e (in-vector (struct->vector x) 1)])
           (reduce acc (f e)))]
        [(hash? x)
         (for/fold ([acc (reduce)]) ([e (in-hash-values x)])
           (reduce acc (f e)))]
        [(box? x) (f (unbox x))]
        [else (reduce)]))

;; tree-reduce : T (T -> Y) (Y ... -> Y) [Boolean] -> Y
(define (tree-reduce x pre-f reduce [root? #t])
  (define (loop x) (pre-f x (lambda () (loop* x))))
  (define (loop* x) (child-reduce x loop reduce))
  (if root? (loop x) (loop* x)))

;; tree-reduce : T (T -> Y) (and (-> Y) (Y Y -> Y)) [Boolean] -> Y
(define (tree-reduce-left x pre-f reduce [root? #t])
  (define (loop x) (pre-f x (lambda () (loop* x))))
  (define (loop* x) (child-reduce-left x loop reduce))
  (if root? (loop x) (loop* x)))

;; ------------------------------------------------------------

;; child-foldl : F[X] (X Y -> Y) Y -> Y
(define (child-foldl x f acc)
  (cond [(pair? x) ;; LEFT
         (let pairloop ([x x] [acc acc])
           (if (pair? x)
               (pairloop (cdr x) (f (car x) acc))
               (if (null? x) acc (f x acc))))]
        [(vector? x)
         (for/fold ([acc acc]) ([e (in-vector x)])
           (f e acc))]
        [(prefab-struct-key x)
         (for/fold ([acc acc]) ([e (in-vector (struct->vector x) 1)])
           (f e acc))]
        [(hash? x)
         (for/fold ([acc acc]) ([e (in-hash-values x)])
           (f e acc))]
        [(box? x)
         (f (unbox x) acc)]
        [else acc]))

;; tree-foldl : T (X Y ([Y] -> Y) -> Y) Y -> Y
(define (tree-foldl x pre-f acc [root? #t])
  (define (loop x acc) (pre-f x acc (lambda ([acc acc]) (loop* x acc))))
  (define (loop* x acc) (child-foldl x loop acc))
  (if root? (loop x acc) (loop* x acc)))

;; ------------------------------------------------------------

;; child-ormap : T (T -> X/#f) -> X/#f
(define (child-ormap x f)
  (cond [(pair? x)
         (let pairloop ([x x])
           (if (pair? x)
               (or (f (car x)) (pairloop (cdr x)))
               (if (null? x) #f (f x))))]
        [(vector? x)
         (for/or ([e (in-vector x)]) (f e))]
        [(prefab-struct-key x)
         (for/or ([e (in-vector (struct->vector x) 1)]) (f e))]
        [(hash? x)
         (for/or ([e (in-hash-values x)]) (f e))]
        [(box? x)
         (f (unbox x))]
        [else #f]))

;; tree-ormap : T (T -> X/#f) -> X/#f
(define (tree-ormap x pre-f [root? #t])
  (define (loop x) (pre-f x (lambda () (loop* x))))
  (define (loop* x) (child-ormap x loop))
  (if root? (loop x) (loop* x)))
