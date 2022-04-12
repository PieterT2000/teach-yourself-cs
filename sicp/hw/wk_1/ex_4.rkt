#lang planet dyoo/simply-scheme:2

(define (buzz n)
  (cond ((equal? (remainder n 7) 0) 'buzz)
        ((member? 7 n) 'buzz)
        (else n)))

; ----------------
; Hw problem 4
; ----------------


(define (ordered? numbers)
  (define (iter numbers index)
  (cond ((> (item (- index 1) numbers) (item index numbers)) #f)
        ((= (length numbers) index) #t)
        (else (iter numbers (+ index 1)))))

  (display numbers) (display #\space) (display '->) (display '#\space)
  (if (= (length numbers) 1)
      #t
      (iter numbers 2)))


(ordered? '(2 5 4 5))
(ordered? '(1 5 6 22))
(ordered? '(2))
(ordered? '(2 9))
(ordered? '(5 1))