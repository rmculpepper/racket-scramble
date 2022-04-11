;; Copyright 2022 Ryan Culpepper
;; Licensed under the Apache License, Version 2.0

#lang racket/base
(provide (all-defined-out))

;; ------------------------------------------------------------
;; AST

(module ast racket/base
  (require (rename-in data/integer-set [count iset:count] [foldr iset:foldr]))
  (provide (all-defined-out))

  ;; regexp structs
  (struct re:inject (s) #:prefab)
  (struct re:or (res) #:prefab)
  (struct re:cat (res) #:prefab)
  (struct re:repeat (re lo hi) #:prefab)
  (struct re:report (re) #:prefab)
  (struct re:^ () #:prefab)
  (struct re:$ () #:prefab)
  (struct re:mode (modes re) #:prefab)
  (struct re:test (test re1 re2) #:prefab)
  (struct re:matched? (n) #:prefab)
  (struct re:charset (isetc) #:prefab)
  (struct re:uniprop (sat? prop) #:prefab)

  (define (lookahead? v)
    (or (look:match? v) (look:match-preceding? v)))

  (struct look:match (sat? re) #:prefab)
  (struct look:match-preceding (sat? re) #:prefab)
  (struct test:matched? (n) #:prefab)

  ;; Not represented:
  ;; - (?> RE)

  (define (make-re:or res)
    (cond [(= (length res) 1) (car res)]
          [(and (pair? res) (andmap re:charset? res))
           (define isets (for/list ([re (in-list res)]) (make-integer-set (re:charset-isetc re))))
           (re:charset (integer-set-contents (iset:union* isets)))]
          [else (re:or res)]))
  (define (make-re:cat res)
    (if (= (length res) 1) (car res) (re:cat res)))

  (define empty-iset (make-range))
  (define allchars-iset (union (make-range 0 #xD7FF) (make-range #xE000 #x10FFFF)))

  (define (iset=? is1 is2) (equal? (integer-set-contents is1) (integer-set-contents is2)))
  (define (iset:union* isets) (foldl union empty-iset isets))
  (define (iset:intersect* isets) (foldl intersect allchars-iset isets))

  (define posix-names
    ;; class name should follow all other classes that contain it: eg, upper follows alpha
    '(ascii
      print graph word alnum
      alpha upper lower
      xdigit digit
      space blank
      cntrl))
  (define (make-px-iset px)
    (for/fold ([iset (make-range)]) ([i (in-range 256)])
      (if (regexp-match? px (string (integer->char i)))
          (union iset (make-range i))
          iset)))
  (define posix-name=>iset
    (for/hash ([name (in-list posix-names)])
      (values name (make-px-iset (pregexp (format "^[[:~a:]]$" name))))))

  (define unicode-props
    '(
      Ll Lu Lt Lm L& Lo L
      Nd Nl No N
      Ps Pe Pi Pf Pc Pd Po P
      Mn Mc Me M
      Sc Sk Sm So S
      Zl Zp Zs Z
      Cc Cf Cs Cn Co C
      ))

  (begin))

;; ------------------------------------------------------------
;; Generation

(module codegen racket/base
  (require racket/match
           racket/string
           (submod ".." ast)
           (rename-in data/integer-set [count iset:count] [foldr iset:foldr]))
  (provide (all-defined-out))

  (define (emit-regexp re) ;; emits regexp
    (match re
      [(re:or res)
       (string-join (map emit-regexp res) "|")]
      [(re:inject s)
       s]
      [_ (emit-pces re #f)]))

  (define (emit-pces re [retry? #t]) ;; emits pces
    (match re
      [(re:cat res)
       (string-join (map emit-pces res) "")]
      [_ (emit-pce re retry?)]))

  (define (emit-pce re [retry? #t]) ;; emits pce/repeat
    (match re
      [(re:repeat re 0 +inf.0)
       (format "~a*" (nonempty (emit-atom re)))]
      [(re:repeat re 1 +inf.0)
       (format "~a+" (nonempty (emit-atom re)))]
      [(re:repeat re 0 1)
       (format "~a?" (nonempty (emit-atom re)))]
      ;; -- pregexp --
      [(re:repeat re n n)
       (format "~a{~a}" (nonempty (emit-atom re)) n)]
      [(re:repeat re n +inf.0)
       (format "~a{~a,}" (nonempty (emit-atom re)) n)]
      [(re:repeat re 0 n)
       (format "~a{,~a}" (nonempty (emit-atom re)) n)]
      [(re:repeat re m n)
       (format "~a{~a,~a}" (nonempty (emit-atom re)) m n)]
      ;; ----
      [_ (emit-atom re retry?)]))

  (define (emit-atom re [retry? #t]) ;; emits atom
    (match re
      [(re:report re)
       (format "(~a)" (emit-regexp re))]
      [(re:charset isetc)
       (emit-charset isetc)]
      [(re:^) "^"]
      [(re:$) "$"]
      [(re:mode (? string? modes) re)
       (format "(?~a:~a)" modes (emit-regexp re))]
      [(? lookahead? look)
       (emit-look look)]
      [(re:test tst re1 (re:cat '()))
       (format "(?~a~a)" (emit-test tst) (emit-pces re1))]
      [(re:test tst re1 re2)
       (format "(?~a~a|~a)" (emit-test tst) (emit-pces re1) (emit-pces re2))]
      ;; -- pregexp --
      [(re:matched? n)
       (format "\\~a" n)]
      [(re:uniprop #t prop)
       (format "\\p{~a}" prop)]
      [(re:uniprop #f prop)
       (format "\\P{~a}" prop)]
      ;; ----
      [_
       (if retry?
           (format "(?:~a)" (emit-regexp re))
           (error 'emit-atom "not an atom: ~e" re))]))

  (define (emit-look look)
    (match look
      [(look:match #t re)
       (format "(?=~a)" (emit-regexp re))]
      [(look:match #f re)
       (format "(?!~a)" (emit-regexp re))]
      [(look:match-preceding #t re)
       (format "(?<=~a)" (emit-regexp re))]
      [(look:match-preceding #f re)
       (format "(?<!~a)" (emit-regexp re))]))

  (define (emit-test tst)
    (match tst
      [(test:matched? n)
       (format "(~a)" n)]
      [(? lookahead? look)
       (emit-look look)]))

  (define (nonempty s)
    (if (equal? s "") "(?:)" s))

  ;; ------------------------------------------------------------

  (define (emit-charset isetc)
    (define iset (make-integer-set isetc))
    (cond [(= (iset:count iset) 0) (error 'emit-charset "empty character set")]
          [(= (iset:count iset) 1) (emit-ci (get-integer iset) #f)]
          [(iset=? iset allchars-iset) "."]
          ;; FIXME: do \p{prop} ??
          [else
           (define co-iset (subtract allchars-iset iset))
           (define ps (emit-is iset))
           (define co-ps (emit-is co-iset))
           (cond [(< (string-length co-ps) (string-length ps))
                  (format "[^~a]" co-ps)]
                 [else (format "[~a]" ps)])]))

  (define (emit-is is0)
    (define-values (is output)
      (for/fold ([is is0] [acc null]) ([name (in-list posix-names)])
        (define name-is (hash-ref posix-name=>iset name))
        (cond [(subset? name-is is)
               (values (subtract is name-is) (cons (format "[:~a:]" name) acc))]
              [else (values is acc)])))
    (string-append (string-join output "")
                   (string-join (map emit-ivl (integer-set-contents is)) "")))

  (define (emit-ivl ivl)
    (match-define (cons lo hi) ivl)
    (cond [(= lo hi) (emit-ci lo #t)]
          [else (format "~a-~a" (emit-ci lo #t) (emit-ci hi #t))]))

  (define (emit-ci ci in-range?)
    (define c (integer->char ci))
    (cond [(char:literal? c in-range?) (string c)]
          [else (format "\\~a" (string c))]))

  ;; -- pregexp --
  (define (char:literal? c in-range?)
    (not (or (for/or ([badc (in-string "()*+?[]{}.^|\\")]) (eqv? c badc))
             (and in-range? (for/or ([badc (in-string "-")]) (eqv? c badc))))))

  (begin))

;; ------------------------------------------------------------
;; Syntax

(module syntax racket/base
  (require racket/base
           racket/syntax
           syntax/parse
           syntax/datum
           syntax/transformer
           (rename-in data/integer-set [count iset:count] [foldr iset:foldr])
           (submod ".." ast)
           (submod ".." codegen))
  (provide (all-defined-out))

  (struct RE-binding (re px)
    #:property prop:procedure
    (lambda (self id)
      (define px (RE-binding-px self))
      ((make-variable-like-transformer (datum->syntax id px)) id)))

  (define (check-RE ast)
    (define ps (emit-regexp ast))
    (define px (pregexp ps (lambda (err) (wrong-syntax #f "~a\n  generated pregexp: ~a" err ps))))
    (values ast px))

  (define-syntax-class RE
    #:attributes (ast)
    #:datum-literals (inject or cat repeat * + ? report ^ $ mode test unicode not chars)
    (pattern (~and :RE-name ~!))
    (pattern s:string
             #:attr ast (string->re-ast (syntax->datum #'s)))
    (pattern (inject s:string)
             #:attr ast (re:inject (syntax->datum #'s)))
    (pattern (or re:RE ...+)
             #:attr ast (make-re:or (datum (re.ast ...))))
    (pattern (cat re:RE ...)
             #:attr ast (make-re:cat (datum (re.ast ...))))
    (pattern (repeat re:RE)
             #:attr ast (re:repeat (datum re.ast) 0 +inf.0))
    (pattern (repeat re:RE n:nat)
             #:attr ast (let ([n (syntax->datum #'n)])
                          (re:repeat (datum re.ast) n n)))
    (pattern (repeat re:RE m:nat n:nat)
             #:attr ast (let ([m (syntax->datum #'m)]
                              [n (syntax->datum #'n)])
                          (re:repeat (datum re.ast) m n)))
    (pattern (repeat re:RE m:nat +inf.0)
             #:attr ast (let ([m (syntax->datum #'m)])
                          (re:repeat (datum re.ast) m +inf.0)))
    (pattern (* re:RE)
             #:attr ast (re:repeat (datum re.ast) 0 +inf.0))
    (pattern (+ re:RE)
             #:attr ast (re:repeat (datum re.ast) 1 +inf.0))
    (pattern (? re:RE)
             #:attr ast (re:repeat (datum re.ast) 0 1))
    (pattern (report re:RE)
             #:attr ast (re:report (datum re.ast)))
    (pattern ^ #:attr ast (re:^))
    (pattern $ #:attr ast (re:$))
    (pattern (mode modes:string re:RE)
             #:attr ast (re:mode (syntax->datum #'modes) (datum re.ast)))
    (pattern (test t:retest re:RE)
             #:attr ast (re:test (datum t.ast) (datum re.ast) (re:cat null)))
    (pattern (test t:retest re1:RE re2:RE)
             #:attr ast (re:test (datum t.ast) (datum re1.ast) (datum re2.ast)))
    (pattern :lookahead)
    (pattern (unicode prop:string)
             #:attr ast (re:uniprop #t (syntax->datum #'prop)))
    (pattern (unicode (not prop:string))
             #:attr ast (re:uniprop #f (syntax->datum #'prop)))
    (pattern (chars cs:char-class ...)
             #:attr iset (iset:union* (datum (cs.ast ...)))
             #:fail-when (= 0 (iset:count (datum iset))) "empty character class"
             #:attr ast (re:charset (integer-set-contents (datum iset)))))

  (define-syntax-class RE-name
    #:attributes (ast)
    (pattern (~var name (static RE-binding? "name defined with `define-RE`"))
             #:attr ast (RE-binding-re (datum name.value))))

  (define-syntax-class retest
    #:attributes (ast)
    #:datum-literals (matched?)
    (pattern :lookahead)
    (pattern (matched? n:nat)
             #:attr ast (re:matched? (syntax->datum #'n))))

  (define-syntax-class lookahead
    #:datum-literals (look look-back not)
    (pattern (look (not ~! re:RE))
             #:attr ast (look:match #f (datum re.ast)))
    (pattern (look re:RE)
             #:attr ast (look:match #t (datum re.ast)))
    (pattern (look-back (not ~! re:RE))
             #:attr ast (look:match-preceding #f (datum re.ast)))
    (pattern (look-back re:RE)
             #:attr ast (look:match-preceding #t (datum re.ast))))

  (define-syntax-class char-class
    #:attributes (ast)  ;; IntegerSet
    #:datum-literals (union intersect complement)
    (pattern (union cs:char-class ...)
             #:attr ast (iset:union* (datum (cs.ast ...))))
    (pattern (intersect cs:char-class ...)
             #:attr ast (iset:intersect* (datum (cs.ast ...))))
    (pattern (complement cs:char-class ...)
             #:attr ast (subtract allchars-iset (iset:union* (datum (cs.ast ...)))))
    (pattern (~and name:RE-name ~!)
             #:fail-unless (re:charset? (datum name.ast)) "not bound to character set"
             #:attr ast (make-integer-set (re:charset-isetc (datum name.ast))))
    (pattern posix-name:id
             #:attr ast (hash-ref posix-name=>iset (syntax->datum #'posix-name) #f)
             #:fail-unless (datum ast) "expected POSIX character class name")
    (pattern s:string
             #:attr ast (iset:union* (for/list ([c (in-string (syntax->datum #'s))])
                                       (make-range (char->integer c)))))
    (pattern r:char-range
             #:attr ast (datum r.ast))
    (pattern c:char/codepoint
             #:attr ast (make-range (datum c.ast))))

  (define-syntax-class char-range
    #:attributes (ast) ;; IntegerSet
    (pattern [lo:char/codepoint hi:char/codepoint]
             #:attr ast (make-range (datum lo.ast) (datum hi.ast))))

  (define-syntax-class char/codepoint
    #:attributes (ast) ;; Nat
    #:datum-literals ()
    (pattern n:nat
             #:attr ast (syntax->datum #'n)
             #:fail-unless (or (<= 0 (datum ast) #xD7FF) (<= #xE000 (datum ast) #x10FFFF))
             "expected valid Unicode codepoint integer")
    (pattern c:char
             #:attr ast (char->integer (syntax->datum #'c))))

  (define (string->re-ast s)
    (make-re:cat
     (for/list ([c (in-string s)])
       (re:charset (integer-set-contents (make-range (char->integer c)))))))

  (define (char->charset c)
    (make-range (char->integer c)))
 
  (begin))
