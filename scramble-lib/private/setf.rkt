;; Copyright 2020 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/promise
                     racket/struct-info
                     syntax/id-table))
(provide setf!
         updatef!
         incf!
         pushf!
         declare-struct-setf)

;; Limitations:
;; - Won't work with contracts.
;; - Won't work with macros that expand into protected application forms.

(begin-for-syntax
  ;; Setter = (Syntax Mode (Listof Syntax) -> Syntax)
  ;; Mode = (U 'set 'update)
  (define getters (make-free-id-table)) ;; Id => Setter

  (define (add-setf-functions! src getter-ids setters)
    (map (lambda (getter-id setter)
           (when (free-id-table-ref getters getter-id #f)
             (raise-syntax-error #f "already defined as setf function" src getter-id))
           (free-id-table-set! getters getter-id setter))
         getter-ids setters))

  (define ((make-setter arity setter-id) ostx getter-id mode args)
    (unless (= arity (length args))
      (raise-syntax-error #f "wrong number of arguments" ostx))
    (with-syntax ([setter setter-id]
                  [getter getter-id]
                  [(arg ...) args]
                  [(tmp ...) (generate-temporaries args)])
      (case mode
        [(set)
         #'(let ([tmp arg] ...) (lambda (v) (setter tmp ... v)))]
        [(update)
         #'(let ([tmp arg] ...) (lambda (f) (setter tmp ... (f (getter tmp ...)))))])))

  (define ((make-hashlike-setter setter-id updater-id) ostx getter-id mode args)
    (case mode
      [(set)
       (unless (= 2 (length args))
         (raise-syntax-error #f "wrong number of arguments" ostx))
       (with-syntax ([(arg ...) args] [(tmp ...) (generate-temporaries args)])
         #`(let ([tmp arg] ...) (lambda (v) (#,setter-id tmp ... v))))]
      [(update)
       (syntax-case args ()
         [(arg1 arg2)
          #`(let ([tmp1 arg1] [tmp2 arg2])
              (lambda (f) (#,updater-id tmp1 tmp2 f)))]
         [(arg1 arg2 arg3)
          #`(let ([tmp1 arg1] [tmp2 arg2] [tmp3 arg3])
              (lambda (f) (#,updater-id tmp1 tmp2 f tmp3)))]
         [_ (raise-syntax-error #f "wrong number of arguments" ostx)])]))

  (define-syntax-class getter
    #:attributes (tx)
    #:description "reference to declared setf function"
    (pattern x:id
             #:attr tx (free-id-table-ref getters #'x #f)
             #:when (attribute tx)))

  (define-syntax-class lvalue
    #:attributes (setter updater)
    #:description "setf-able location"
    (pattern (~var x (static set!-transformer? "set! transformer"))
             #:with setter  #'(lambda (v) (set! x v))
             #:with updater #'(lambda (f) (set! x (f x))))
    ;; FIXME: rename-transformer?
    (pattern (~and e:expr ~!)
             #:with z:expanded-lvalue (local-expand #'e 'expression (list #'#%plain-app))
             #:attr tx (lambda (mode) ((attribute z.tx) #'e mode))
             #:attr setter  (delay ((attribute tx) 'set))
             #:attr updater (delay ((attribute tx) 'update))))
  (define-syntax-class expanded-lvalue
    #:attributes (tx)
    #:description #f
    #:literal-sets (kernel-literals)
    (pattern (~or* var:id (#%top . var:id))
             #:attr tx (lambda (ostx mode)
                         (case mode
                           [(set)    #'(lambda (v) (set! var v))]
                           [(update) #'(lambda (f) (set! var (f var)))])))
    (pattern (#%plain-app g:getter arg ...)
             #:fail-when
             (for/or ([subexpr (in-list (syntax->list #'(g arg ...)))])
               (and (syntax-tainted? subexpr) subexpr))
             "setf-able location is protected (would be tainted)"
             #:attr tx (lambda (ostx mode)
                         ((attribute g.tx) ostx #'g mode (syntax->list #'(arg ...))))))

  (free-id-table-set! getters #'unbox (make-setter 1 #'set-box!))
  (free-id-table-set! getters #'vector-ref (make-setter 2 #'vector-set!))
  (free-id-table-set! getters #'string-ref (make-setter 2 #'string-set!))
  (free-id-table-set! getters #'bytes-ref (make-setter 2 #'bytes-set!))
  (free-id-table-set! getters #'hash-ref (make-hashlike-setter #'hash-set! #'hash-update!))
  (void))

(define-syntax setf!
  (syntax-parser
    [(_ l:lvalue e:expr)
     #'(#%plain-app l.setter e)]))

#;
(define-syntax psetf!
  (syntax-parser
    [(_ (~seq l:lvalue e:expr) ...)
     (with-syntax ([(ltmp ...) (generate-temporaries #'(l ...))]
                   [(etmp ...) (generate-temporaries #'(e ...))])
       #'(let ((~@ [ltmp l.setter] [etmp e]) ...) (ltmp etmp) ...))]))

(define-syntax updatef!
  (syntax-parser
    [(_ l:lvalue e:expr)
     #'(#%plain-app l.updater e)]))

(define-syntax incf!
  (syntax-parser
    [(_ loc:expr (~optional amt))
     #:declare amt (expr/c #'number?)
     #'(updatef! loc (~? (lambda (v) (+ v amt.c)) add1))]))

(define-syntax-rule (pushf! loc v)
  (updatef! loc (lambda (vs) (cons v vs))))

(define-syntax declare-setf
  (syntax-parser
    [(_ [getter:id arity:nat setter:id] ...)
     #'(begin-for-syntax
         (add-setf-functions! (quote-syntax #,this-syntax)
                              (list (quote-syntax getter) ...)
                              (list (make-setter 'arity (quote-syntax setter)) ...)))]))

(define-syntax declare-struct-setf
  (syntax-parser
    [(_ (~var s (static struct-info? "struct info"))
        (~optional (~seq (~and #:include-super include-super?))))
     (define si (extract-struct-info (attribute s.value)))
     (define accessors (list-ref si 3))
     (define mutators (list-ref si 4))
     (define super (list-ref si 5))
     (define super-accessors-count
       (cond [(and (not (attribute include-super?)) (identifier? super))
              (define super-si (extract-struct-info (syntax-local-value super (lambda () #f))))
              (length (list-ref super-si 3))]
             [else 0]))
     (define entries
       (for/list ([accessor (in-list accessors)]
                  [mutator  (in-list mutators)]
                  [_index (in-range (- (length accessors) super-accessors-count))]
                  #:when (and (identifier? accessor) (identifier? mutator)))
         (cons #`(quote-syntax #,accessor)
               #`(make-setter 1 (quote-syntax #,mutator)))))
     #`(begin-for-syntax
         (add-setf-functions!
          (quote-syntax #,this-syntax)
          (list #,@(map car entries))
          (list #,@(map cdr entries))))]))

;; ========================================

(module+ test

  (struct point (x y) #:mutable #:transparent)
  (struct point3 point (z) #:mutable #:transparent)

  (declare-struct-setf point)
  (declare-struct-setf point3 #;#:include-super)

  (define p3 (point3 1 2 3))
  (setf! (point-x p3) 10)
  (setf! (point3-z p3) 30)
  p3

  (define b (box 0))
  (define bl (box null)) 
  (setf! (unbox b) 2)
  (pushf! (unbox bl) 'a)
  b
  bl)
