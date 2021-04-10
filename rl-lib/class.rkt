#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/class)
(provide init-private)

(define-syntax (init-private stx)

  (define-syntax-class init-decl
    #:attributes (internal external default)
    (pattern internal:id #:with external #'internal #:attr default #f)
    (pattern (:renamed) #:attr default #f)
    (pattern (:maybe-renamed default:expr)))
  (define-syntax-class maybe-renamed
    #:attributes (internal external)
    (pattern internal:id #:with external #'internal)
    (pattern :renamed))
  (define-syntax-class renamed
    #:attributes (internal external)
    (pattern (internal:id external:id)))

  (define-syntax-class init-private-decl
    #:attributes (code)
    (pattern :init-decl
             #:with code (with-syntax ([(tmp-internal) (generate-temporaries #'(internal))])
                           #'(begin (init ((tmp-internal external) (~? default)))
                                    (define internal tmp-internal)))))

  (syntax-parse stx
    [(_ i:init-private-decl ...)
     #'(begin i.code ...)]))
