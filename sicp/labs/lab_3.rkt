#lang planet dyoo/simply-scheme:2

; Ex. 1

; k = kinds of coins
(define (first-denomination k)
  (cond ((= k 2) 1)
        ((= k 1) 5)))

(define (cc amount k)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= k 0)) 0)
        (else (+ (cc (- amount (first-denomination k)) k) (cc amount (- k 1))))))

(define (change-count amount)
  (cc amount 2))

;(trace change-count)
;(trace cc)

(change-count 5)

; Ex. 2

#|
If the order is reversed every left branch will branch off to the left till k = 0.
So for every penny it will also loop through all the other denominations. 
|#

; Ex. 3
(define (new-cc amount denoms)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (empty? denoms)) 0)
        (else (+ (new-cc amount (bf denoms)) (new-cc (- amount (first denoms)) denoms)))))

(define (change-count-2 amount)
  (new-cc amount '(50 25 10 5 1)))

;(trace change-count)
;(trace cc)

(change-count-2 100)

; Ex. 4

(define (type-check f pred arg)
  (if (pred arg)
      (f arg)
      #f))
(type-check sqrt number? 'hello)
(type-check sqrt number? 4)

; Ex. 5
(define (make-safe f pred)
  (lambda (arg) (type-check f pred arg)))
(define safe-sqrt (make-safe sqrt number?))
(safe-sqrt 'hello)
(safe-sqrt 4)