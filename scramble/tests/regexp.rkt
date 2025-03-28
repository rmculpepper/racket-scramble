;; Copyright 2022 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require rackunit
         scramble/regexp)

(define use-posix-classes? #f)

(define-RE as (* "a"))
(define-RE bbs (* "bb"))

(check-equal? (px (or as bbs)) #px"a*|(?:bb)*")
(check-equal? (px (cat as bbs)) #px"a*(?:bb)*")

(check-equal? (px (repeat "abc" 2 5)) #px"(?:abc){2,5}")
(check-equal? (px (repeat "abc" 2)) #px"(?:abc){2}")
(check-equal? (px (repeat "abc" 2 +inf.0)) #px"(?:abc){2,}")
(check-equal? (px (repeat "abc" 0 5)) #px"(?:abc){,5}")
(check-equal? (px (repeat "abc" 1 +inf.0)) #px"(?:abc)+")
(check-equal? (px (report "abc")) #px"(abc)")
(check-equal? (px ^ (report "abc")) #px"^(abc)")
(check-equal? (px (report (cat "abc" (or "d" $)))) #px"(abc(?:d|$))")
(check-equal? (px (mode "i" "abc")) #px"(?i:abc)")
(if use-posix-classes?
    (check-equal? (px (test (look (or "a" "bc")) (* (chars alpha)))) #px"(?(?=a|bc)[[:alpha:]]*)")
    (check-equal? (px (test (look (or "a" "bc")) (* (chars alpha)))) #px"(?(?=a|bc)[A-Za-z]*)"))
(check-equal? (px (unicode "Ll")) #px"\\p{Ll}")
(check-equal? (px (unicode (not "Ll"))) #px"\\P{Ll}")
(check-equal? (px "abc") #px"abc")
(check-equal? (px "[abc]*") #px"\\[abc\\]\\*")

(cond [use-posix-classes?
       (check-equal? (px (chars [#\A #\Z])) #px"[[:upper:]]")
       (check-equal? (px (chars [#\A #\Z] "rst")) #px"[A-Zr-t]")
       (check-equal? (px (chars upper lower)) #px"[[:alpha:]]")]
      [else
       (check-equal? (px (chars [#\A #\Z])) #px"[A-Z]")
       (check-equal? (px (chars [#\A #\Z] "rst")) #px"[A-Zr-t]")
       (check-equal? (px (chars upper lower)) #px"[A-Za-z]")])

(check-equal? (px "$") #px"\\$")
(check-equal? (px (chars "$a")) #px"[$a]")
