#lang planet dyoo/simply-scheme:2
(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

; Square Roots by Newton's method
(define (square x)
  (* x x))

(define (good-enough? x guess)
  (< (abs (- (square guess) x)) 0.001))

(define (new-guess x guess)
  (/ (+ guess (/ x guess)) 2))

#|
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
|#

(define (sqrt-iter x guess)
  (if (good-enough? x guess)
      guess
      (sqrt-iter x (new-guess x guess))))

(define (sqrt x)
  (sqrt-iter x 1.0))
(sqrt 9)

; ----------------
; Hw problem 1
; ----------------

#|
Since the new-if procedure is no special form, arguments will be evaluated first.
This is also called applicative-order evaluation.
This means that the else clause than contains a recursive call will cause an infinite loop.
The program will eventually run out of memory.
The regular if and cond std procedures are special forms and hence predicates will be evaluated first if used properly.
|#