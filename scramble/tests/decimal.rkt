#lang racket/base
(require racket/match
         racket/math
         scramble/decimal
         rackunit)

(check-equal? (string->decimal "123.45")
              (decimal 12345 2))
(check-equal? (string->decimal "0.0012")
              (decimal 12 4))
(check-equal? (string->decimal "0005")
              (decimal 5 0))
(check-equal? (string->decimal ".98")
              (decimal 98 2))
(check-equal? (string->decimal "-.98")
              (decimal -98 2))
(check-equal? (string->decimal "7890")
              (decimal 7890 0))
(check-equal? (string->decimal "7890.")
              (decimal 7890 0))
(check-equal? (string->decimal "12.34500")
              (decimal 1234500 5))

(check-equal? (real->decimal 1/4 3)
              (decimal 250 3))
(check-equal? (real->decimal 1/3 2)
              (decimal 33 2))

(check-equal? (decimal->string (decimal 123 1)) "12.3")
(check-equal? (decimal->string (decimal 123 0)) "123")
(check-equal? (decimal->string (decimal -123 1)) "-12.3")
(check-equal? (decimal->string (decimal -123 3)) "-0.123")

;; rounding (half to even)
(check-equal? (real->decimal 1/8 2)
              (decimal 12 2))
(check-equal? (real->decimal 3/8 2)
              (decimal 38 2))
(check-equal? (decimal-adjust (decimal 2049 2) 0)
              (decimal 20 0))
(check-equal? (decimal-adjust (decimal 2066 2) 0)
              (decimal 21 0))
(check-equal? (decimal-adjust (decimal 2050 2) 0)
              (decimal 20 0))
(check-equal? (decimal-adjust (decimal 2150 2) 0)
              (decimal 22 0))
(check-equal? (decimal-adjust (decimal -2050 2) 0)
              (decimal -20 0))
(check-equal? (decimal-adjust (decimal -2150 2) 0)
              (decimal -22 0))

(check-equal? (decimal->exact (decimal 25 2)) 1/4)
(check-equal? (decimal->exact (decimal -33 1)) -33/10)
(check-equal? (decimal->exact 40/13) 40/13)
(check-equal? (decimal->exact 0.25) (inexact->exact 0.25))
(check-exn exn:fail? (lambda () (decimal->exact +inf.0)))

(check-equal? (decimal->inexact (decimal 25 2)) 0.25)
(check-equal? (decimal->inexact (decimal -33 1)) -3.3)
(check-equal? (decimal->inexact 40/13) (exact->inexact 40/13))
(check-equal? (decimal->inexact +inf.0) +inf.0)

(check-equal? (decimal-adjust (decimal 123 1) 2)
              (decimal 1230 2))
(check-equal? (decimal-adjust (decimal -456 3) 3)
              (decimal -456 3))

;; decimal+
(check-equal? (decimal+ (string->decimal "1.00") pi (string->decimal "0.0007"))
              (string->decimal "4.1407"))
(check-equal? (decimal+ pi (string->decimal "1.00") (string->decimal "0.0007"))
              (string->decimal "4.1407"))

;; decimal-
(check-equal? (decimal- (string->decimal "10.00") 1/10 (string->decimal "0.005"))
              (string->decimal "9.895"))
(check-equal? (decimal- (string->decimal "10.00") 1/1000)
              (string->decimal "10.00"))

;; decimal*
(check-equal? (decimal* 3 (decimal 33 3))
              (decimal 99 3))
(check-equal? (decimal* (decimal 11 1) (decimal 11 5))
              (decimal 121 6))

(check-true (decimal=? (decimal 123 1) (decimal 12300 3)))
(check-false (decimal=? (decimal 123 1) (decimal 123 0)))
(check-true (decimal<? (decimal 123 1) (decimal 13 0)))
(check-true (decimal<? 12 (decimal 123 1) 13))

(check-equal? (decimal-min (decimal 123 2) (decimal 13 1) (decimal 12 1))
              (decimal 12 1))
(check-equal? (decimal-max (decimal 345 2) (decimal 234 1) (decimal 123 0))
              (decimal 123 0))

(check-equal? (decimal-abs (decimal -123 5))
              (decimal 123 5))
(check-equal? (decimal-abs (decimal 123 5))
              (decimal 123 5))

#|
(check-true (decimal-zero? (decimal 0 5)))
(check-false (decimal-zero? (decimal 1 0)))

(check-true (decimal-negative? (decimal -1 5)))
(check-false (decimal-negative? (decimal 1 5)))

(check-true (decimal-positive? (decimal 1 5)))
(check-false (decimal-positive? (decimal -1 5)))
(check-false (decimal-positive? (decimal 0 5)))
|#
