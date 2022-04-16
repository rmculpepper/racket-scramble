;; Copyright 2020-2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/class
         racket/struct
         "about.rkt")
(provide init-private
         constructor-style-printable<%>
         print-quotable-always<%>
         print-quotable-never<%>
         about<%>)

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

;; ----------------------------------------

(struct print:init (name value)
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (self) (print:init-name self))
   (lambda (self) (list (print:init-value self)))))

(define constructor-style-printable<%>
  (interface* ()
              ([prop:custom-write
                (let ()
                  (define ((emit-fields make-init) self)
                    (define-values (fieldnames fieldvals more?)
                      (send self get-printing-components))
                    (append (for/list ([fieldname (in-list fieldnames)]
                                       [fieldval (in-list fieldvals)])
                              (make-init fieldname fieldval))
                            (if more? (list (unquoted-printing-string "...")) '())))
                  (define (emit-new-class-name self)
                    (string->symbol (format "new ~a" (send self get-printing-class-name))))
                  (define (emit-class-name self)
                    (string->symbol (format "~a" (send self get-printing-class-name))))
                  (define writer
                    (make-constructor-style-printer emit-class-name (emit-fields list)))
                  (define printer
                    (make-constructor-style-printer emit-new-class-name (emit-fields print:init)))
                  (lambda (self out mode)
                    (case mode
                      [(#t #f 1) (writer self out mode)]
                      [else (printer self out mode)])))])
    get-printing-class-name
    get-printing-components))

(define print-quotable-always<%>
  (interface* () ([prop:custom-print-quotable 'always])))

(define print-quotable-never<%>
  (interface* () ([prop:custom-print-quotable 'never])))

;; ----------------------------------------

(define about<%>
  (interface* () ([prop:about (lambda (self) (send self about))])
    about))
