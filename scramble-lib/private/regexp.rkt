;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

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
           (only-in racket/list argmin)
           racket/string
           racket/syntax
           (only-in racket/list make-list)
           (submod ".." ast)
           (rename-in data/integer-set [count iset:count] [foldr iset:foldr]))
  (provide (all-defined-out))

  ;; Note: currently only pregexp generation is supported.  Conversion to regexp
  ;; form is impossible for some REs due to (report _). For example:
  ;;   (repeat (report "a") 2 +inf.0) != #rx"(a)(a)+"
  ;; because the number of reports has changed (1 vs 2).

  ;; Mode = 'px | 'rx | 'rx/no-report

  (define (emit-px re) (emit-regexp 'px re))
  (define (emit-rx re) (emit-regexp 'rx re))

  (define (emit-regexp m re) ;; emits regexp
    (match re
      [(re:or res)
       (string-join (map (λ (re) (emit-regexp m re)) res) "|")]
      [(re:inject s)
       s]
      [_ (emit-pces m re #f)]))

  (define (emit-pces m re [retry? #t]) ;; emits pces
    (match re
      [(re:cat res)
       (string-join (map (λ (re) (emit-pces m re)) res) "")]
      [_ (emit-pce m re retry?)]))

  (define (emit-pce m re [retry? #t]) ;; emits pce/repeat
    (match re
      [(re:repeat re 0 +inf.0)
       (format "~a*" (nonempty (emit-atom m re)))]
      [(re:repeat re 1 +inf.0)
       (format "~a+" (nonempty (emit-atom m re)))]
      [(re:repeat re 0 1)
       (format "~a?" (nonempty (emit-atom m re)))]
      [_ (case m
           [(px) ;; -- pregexp --
            (match re
              [(re:repeat re n n)
               (format "~a{~a}" (nonempty (emit-atom m re)) n)]
              [(re:repeat re n +inf.0)
               (format "~a{~a,}" (nonempty (emit-atom m re)) n)]
              [(re:repeat re 0 n)
               (format "~a{,~a}" (nonempty (emit-atom m re)) n)]
              [(re:repeat re n1 n2)
               (format "~a{~a,~a}" (nonempty (emit-atom m re)) n1 n2)]
              [_ (emit-atom m re retry?)])]
           [else ;; -- regexp --
            (define m 'rx/no-report)
            (match re
              [(re:repeat re n n)
               (define s (nonempty (emit-atom m re)))
               (string-join (make-list n s) "")]
              [(re:repeat re n +inf.0)
               (define s (nonempty (emit-atom m re)))
               (format "~a~a*" (string-join (make-list n s) "") s)]
              [(re:repeat re 0 n)
               (define s (nonempty (emit-atom m re)))
               (for/fold ([acc ""]) ([i (in-range n)])
                 (format "(?:~a~a)?" s acc))]
              [(re:repeat re n1 n2)
               (define s (nonempty (emit-atom m re)))
               (string-append
                (string-join (make-list n1 s) "")
                (for/fold ([acc ""]) ([i (in-range (- n2 n1))])
                  (format "(?:~a~a)?" acc s)))]
              [_ (emit-atom 'rx/no-report re retry?)])])]))

  (define (emit-atom m re [retry? #t]) ;; emits atom
    (match re
      [(re:report re)
       (case m
         [(rx/no-report)
          (wrong-syntax #f "cannot handle report inside of repeat with custom bounds")]
         [else (format "(~a)" (emit-regexp m re))])]
      [(re:charset isetc)
       (emit-charset m isetc)]
      [(re:^) "^"]
      [(re:$) "$"]
      [(re:mode (? string? modes) re)
       (format "(?~a:~a)" modes (emit-regexp m re))]
      [(? lookahead? look)
       (emit-look m look)]
      [(re:test tst re1 (re:cat '()))
       (format "(?~a~a)" (emit-test m tst) (emit-pces m re1))]
      [(re:test tst re1 re2)
       (format "(?~a~a|~a)" (emit-test m tst) (emit-pces m re1) (emit-pces m re2))]
      [_
       (case m
         [(px) ;; -- pregexp --
          (match re
            [(re:matched? n)
             (format "\\~a" n)]
            [(re:uniprop #t prop)
             (format "\\p{~a}" prop)]
            [(re:uniprop #f prop)
             (format "\\P{~a}" prop)]
            ;; ----
            [_ (emit-retry m re retry?)])]
         [else ;; -- regexp --
          (emit-retry m re retry?)])]))

  (define (emit-retry m re retry?)
    (if retry?
        (format "(?:~a)" (emit-regexp m re))
        (error 'emit-regexp "not supported: ~e" re)))

  (define (emit-look m look)
    (match look
      [(look:match #t re)
       (format "(?=~a)" (emit-regexp m re))]
      [(look:match #f re)
       (format "(?!~a)" (emit-regexp m re))]
      [(look:match-preceding #t re)
       (format "(?<=~a)" (emit-regexp m re))]
      [(look:match-preceding #f re)
       (format "(?<!~a)" (emit-regexp m re))]))

  (define (emit-test m tst)
    (match tst
      [(test:matched? n)
       (case m
         [(px) (format "(~a)" n)]
         [else (error 'emit-regexp "not supported (test): ~e" tst)])]
      [(? lookahead? look)
       (emit-look m look)]))

  (define (nonempty s)
    (if (equal? s "") "(?:)" s))

  ;; ------------------------------------------------------------

  (define (emit-charset m isetc)
    (define iset (make-integer-set isetc))
    (cond [(= (iset:count iset) 0) (error 'emit-charset "empty character set")]
          [(= (iset:count iset) 1) (emit-ci m (get-integer iset) #f)]
          [(iset=? iset allchars-iset) "."]
          ;; FIXME: do \p{prop} ??
          [else
           (define alts
             (append (emit-is-alts m iset "" #f)
                     (emit-is-alts m (subtract allchars-iset iset) "^" #t)))
           (format "[~a]" (argmin string-length alts))]))

  (define use-posix-classes? #f)

  (define (emit-is-alts m is prefix catch?)
    (define (base is prefix)
      (with-handlers ([exn:fail? (lambda (e) (if catch? null (raise e)))])
        (list (string-append prefix (emit-is m is)))))
    (case m
      [(px)
       (cond [use-posix-classes?
              (let loop ([names posix-names] [is is] [prefix prefix])
                (match names
                  [(cons name names)
                   (define name-is (hash-ref posix-name=>iset name))
                   (append (cond [(subset? name-is is)
                                  (loop names
                                        (subtract is name-is)
                                        (format "~a[:~a:]" prefix name))]
                                 [else null])
                           (loop names is prefix))]
                  ['() (base is prefix)]))]
             [else (base is prefix)])]
      [else (base is prefix)]))

  (define (emit-is m is)
    (string-join (map (λ (ivl) (emit-ivl m ivl)) (integer-set-contents is)) ""))

  (define (emit-ivl m ivl)
    (match-define (cons lo hi) ivl)
    (cond [(= lo hi) (emit-ci m lo #t)]
          [(= hi (add1 lo)) (format "~a~a" (emit-ci m lo #t) (emit-ci m hi #t))]
          [else (format "~a-~a" (emit-ci m lo #t) (emit-ci m hi #t))]))

  (define (emit-ci m ci in-range?)
    (define c (integer->char ci))
    (cond [(char:literal? c m in-range?) (string c)]
          [(or (not in-range?) (eq? m 'px)) (format "\\~a" (string c))]
          [else
           ;; I am not currently motivated to implement the bizarre rules for
           ;; including special characters in regexp-style character sets.
           (wrong-syntax #f "special character in regexp-style range~a\n  char: ~e"
                         ";\n use `px` instead" ci)]))

  (define (char:literal? c m in-range?)
    (not (char:special? c m in-range?)))

  (define (char:special? c m in-range?)
    (define (in-string? s) (for/or ([badc (in-string s)]) (eqv? c badc)))
    (cond [in-range?
           (case m
             [(px) (in-string? "[]-^\\")]
             [else (in-string? "[]-^")])]
          [else
           (case m
             [(px) (in-string? "()*+?[]{}.^$|\\")]
             [else (in-string? "()*+?[].^$|\\")])]))

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

  (define (check-RE m ast)
    (define ((fail s) err)
      (define what (case m [(px) 'pregexp] [else 'regexp]))
      (wrong-syntax #f "~a\n  generated ~a: ~a" err what s))
    (define s (emit-regexp m ast))
    (define x (case m [(px) (pregexp s (fail s))] [else (regexp s (fail s))]))
    (values ast x))

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
