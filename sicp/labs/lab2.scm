#lang planet dyoo/simply-scheme:2

; -----------------------
; Ex. 2
; -----------------------

(define (substitute sen old new)
  (cond ((empty? sen) '())
        ((equal? (first sen) old)
         (se new (substitute (bf sen) old new)))
        (else (se (first sen) (substitute (bf sen) old new)))))
(substitute '(she loves you yeah yeah yeah) 'yeah 'maybe)

; -----------------------
; Ex. 3
; -----------------------

; g has zero args. lambda

; -----------------------
; Ex. 4
; -----------------------

; f -> f must be a defined primitive (either data or procedure). Will evaluate to whatever the value of f is. In case of a procedure, the address of the procedure will be returned.
; (f) -> f must be a procedure with zero arguments. Expression will evaluate to whatever the return value of f is.
; (f 3) f must be a procedure with 1 argument. ""
; ((f)) f must be a higher order function with zero arguments that returns a lambda function with zero args. Will evaluate to the return value of the returned lambda func.
; (((f)) 3) f is a higher order function with zero args that returns a higher order function with zero args that returns a lambda function with 1 argument. ""

; -----------------------
; Ex. 5
; -----------------------
(define (1+ x)
  (+ 1 x))

(define (t f)
  (lambda (x) (f (f (f x)))))

((t 1+) 0)
((t (t 1+)) 0)
(((t t) 1+) 0)
; 3
; 9
; 27

; Ex. 6 -> same results as above

; -----------------------
; Ex. 7
; -----------------------

(define (make-tester w)
  (lambda (x) (equal? w x)))
((make-tester 'hal) 'hal)
((make-tester 'hal) 'cs61a)
(define sicp-author-and-astronomer? (make-tester 'gerry))
(sicp-author-and-astronomer? 'hal)
(sicp-author-and-astronomer? 'gerry)
