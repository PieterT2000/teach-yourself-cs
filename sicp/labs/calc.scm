#lang planet dyoo/simply-scheme:2

;; Scheme calculator -- evaluate simple expressions

; The read-eval-print loop:

(define (calc)
  (display "calc: ")
  (flush-output)
  (print (calc-eval (read)))
  (newline)
  (calc))

; Evaluate an expression:

(define (calc-eval exp)
  (cond ((or (number? exp) (word? exp)) exp)
	((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
	(else (error "Calc: bad expression:" exp))))
;(trace calc-eval)

; Apply a function to arguments:

(define (fn-helper fn args)
  (if (= (count args) 1)
      (fn (car args))
      (fn args)))

(define (calc-apply fn args)
  (cond ((eq? fn 'first) (fn-helper first args))
        ((or (eq? fn 'butfirst) (eq? fn 'bf)) (fn-helper bf args))
        ((eq? fn 'last) (fn-helper last args))
        ((member? fn '(butlast bl)) (fn-helper bl args))
        ((eq? fn 'word) (string->symbol (accumulate string-append (map symbol->string args))))
        ((eq? fn '+) (accumulate + args))
	((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
			   ((= (length args) 1) (- (car args)))
			   (else (- (car args) (accumulate + (cdr args))))))
	((eq? fn '*) (accumulate * args))
	((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
			   ((= (length args) 1) (/ (car args)))
			   (else (/ (car args) (accumulate / (cdr args))))))
	(else (error "Calc: bad operator:" fn))))
;(trace calc-apply)