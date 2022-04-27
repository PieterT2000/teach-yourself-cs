#lang planet dyoo/simply-scheme:2

; ----------------------------
; Ex. 1
; ----------------------------

; - 1.31a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (increment x)
  (+ x 1))
(define (identity x)
  x)

(define (factorial x)
  (product identity 1 increment x))

(factorial 4)

(define (approximate-pi n)
  (define (term x)
    (let ((c (* 2 x)))
      (* (/ c (- c 1)) (/ c (+ c 1)))))
  (* (product term 1.0 increment n) 2))

(approximate-pi 10000.0)

; - 1.32a
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (product-with-accumulate term a next b)
  (accumulate (lambda (a b) (* a b)) 1 term a next b))

(define (sum term a next b)
  (accumulate (lambda (a b) (+ a b)) 0 term a next b))

(product-with-accumulate identity 1 increment 4)
(sum identity 1 increment 4)

; - 1.33 (a)
(define (filtered-accumulate pred combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((pred a) (combiner (term a) (filtered-accumulate pred combiner null-value term (next a) next b)))
        (else (filtered-accumulate pred combiner null-value term (next a) next b))))

; Prime function copied from https://stackoverflow.com/a/13793084/9713831
(define (prime? n)
  (let loop ((d 2))
    (cond ((< n (* d d)) #t)
          ((zero? (modulo n d)) #f)
          (else (loop (+ d 1))))))

(define (square x) (* x x))

(define (sum-of-squares-of-primes a b)
  (filtered-accumulate prime? (lambda (a b) (+ a b)) 0 square a increment b))

(sum-of-squares-of-primes 1 10)

; - 1.33 (b)
(define (gcd a b)
  (cond ((= a 0) b)
        ((= b 0) a)
        (else (gcd b (remainder a b)))))

(define (product-of-relative-primes n)
  (filtered-accumulate (lambda (i) (= (gcd i n) 1)) (lambda (a b) (* a b)) 1 identity 1 increment (- n 1)))

(product-of-relative-primes 8)

; - 1.40
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

; - 1.41
(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) increment) 5)

; - 1.43
(define (compose f g n)
  (if (= n 1)
      f
      (compose (lambda (x) (f (g x))) g (- n 1))))

(define (repeated f n)
  (compose f f n))
((repeated square 2) 5)

((repeated (lambda (x) (+ x 1)) 3) 1)

; - 1.46
(define (iterative-improve good-enough? improve-guess)
  (define (iter guess x)
    (if (good-enough? guess x)
        guess
        (iter (improve-guess guess x) x)))
  (lambda (guess x) (iter guess x)))

(define (sqrt x)
  (define (avg a b) (/ (+ a b) 2))
  (define (test-fn guess x) (< (abs (- (square guess) x)) 0.001))
  (define (improve-fn guess x) (avg guess (/ x guess)))
  ((iterative-improve test-fn improve-fn) 1.0 x))
(sqrt 9)