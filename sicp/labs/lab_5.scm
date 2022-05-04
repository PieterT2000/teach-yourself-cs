#lang planet dyoo/simply-scheme:2

; Ex. 1

; -- 2.25
(define l1 (list 1 3 (list 5 7) 9))
(define l2 (list (list 7)))
(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;(car (cdaddr l1))
;(caar l2)
;(cadadr (cadadr (cadadr l3)))

; Ex. 2 (-- 2.55)
;''abracadabra -> (quote (quote abracadabra)) -> '(quote abracadabra)
; Car will return 'quote as first word of the sentence

; Ex. 3 (-- 2.27)
(define (deep-reverse l)
  (cond ((or (not (list? l)) (empty? l)) l)
        (else (append (reverse (cdr l)) (list (deep-reverse (car l)))))))
(deep-reverse (list (list 1 2) (list 3 4)))
(deep-reverse (list 1 (list 3 (list 4 5)) (list (list 3) 3 4)))