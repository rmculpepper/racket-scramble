#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse/pre
                     syntax/transformer)
         racket/match)
(provide hash-view)

(begin-for-syntax
  (define ((hash-view-match-expander hvstx) stx)
    (define/with-syntax (name name? (uc-name-f ...)) hvstx)
    (syntax-parse stx
      [(_ fp ...)
       (unless (= (length (syntax->list #'(fp ...)))
                  (length (syntax->list #'(uc-name-f ...))))
         (raise-syntax-error #f
          (format "wrong number of fields for hash-view ~s\n  expected: ~e\n  got: ~e"
                  (syntax-e #'name)
                  (length (syntax->list #'(uc-name-f ...)))
                  (length (syntax->list #'(fp ...))))
          stx))
       #'(? name? (app uc-name-f fp) ...)])))

(define-syntax (hash-view stx)
  (define-syntax-class fieldspec #:attributes (name mk-default ref-default [decl 1])
    (pattern name:id
             #:attr mk-default #f
             #:attr ref-default #f
             #:with (decl ...) null)
    (pattern [name:id #:default default0:expr]
             #:with default1 (local-expand #'default0 'expression null)
             #:with (mk-default ref-default decl ...)
             (syntax-parse #'default1
               #:literals (quote)
               [(quote datum) #'(default1 default1)]
               [_ (with-syntax ([(tmp) (generate-temporaries #'(default1))])
                    #'((tmp) tmp (define (tmp) default1)))])))
  (syntax-parse stx
    [(_ name (f:fieldspec ...))
     (for/fold ([seen-default? #f])
               ([f (in-list (syntax->list #'(f ...)))]
                [default (in-list (attribute f.ref-default))])
       (when (and seen-default? (not default))
         (raise-syntax-error #f "non-optional field following optional field" stx f))
       (or seen-default? (and default #t)))
     (with-syntax ([name? (format-id #'name "~a?" #'name)]
                   [make-name (format-id #'name "make-~a" #'name)]
                   [make-mut-name (format-id #'name "make-mutable-~a" #'name)]
                   [(uc-name-f ...)
                    (for/list ([fname (in-list (syntax->list #'(f.name ...)))])
                      (format-id #'name "unchecked-~a-~a" #'name fname))]
                   [(name-f ...)
                    (for/list ([fname (in-list (syntax->list #'(f.name ...)))])
                      (format-id #'name "~a-~a" #'name fname))])
       #'(begin
           f.decl ... ...
           (define (make-name (~? [f.name f.mk-default] f.name) ...)
             (hasheq (~@ 'f.name f.name) ...))
           (define (make-mut-name (~? [f.name f.mk-default] f.name) ...)
             (let ([h (make-hasheq)]) (hash-set! h 'f.name f.name) ... h))
           (define-match-expander name
             (hash-view-match-expander (quote-syntax (name name? (uc-name-f ...))))
             (make-variable-like-transformer #'make-name))
           (define (name? v)
             (and (hash? v)
                  (or (~? (quote f.ref-default)) (hash-has-key? v 'f.name)) ...
                  #t))
           (define (uc-name-f v) (hash-ref v 'f.name (~? f.ref-default))) ...
           (define (name-f v)
             (unless (name? v) (raise-argument-error 'name-f (symbol->string 'name?) v))
             (uc-name-f v))
           ...))]))

;; ----------------------------------------

(hash-view point (x y [z #:default 0]))

(define p3 (point 1 2 0))
