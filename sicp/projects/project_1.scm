#lang planet dyoo/simply-scheme:2

; ----------------------------
; Programming Project 1: Twenty-One
; ----------------------------

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )

; Ex. 1 
(define (get-totals sent total)
  (if (empty? sent)
      (se total)
      (let ((val (first (first sent))))
        (cond ((member? val 'aA) (se (get-totals (bf sent) (+ total 11)) (get-totals (bf sent) (+ total 1))))
              ((member? val 'jqkJQK) (get-totals (bf sent) (+ total 10)))
              (else (get-totals (bf sent) (+ total val)))))))
;(trace get-totals)

(define (find-best-total totals)
  (define (iter sent best)
    (cond ((empty? sent) best)
          ((= (first sent) 21) 21)
          ((and (> (first sent) best) (< (first sent) 21)) (iter (bf sent) (first sent)))
          ((and (= (count sent) 1) (= best 0)) (iter (bf sent) (first sent)))
          (else (iter (bf sent) best))))
  (iter totals 0))

(define (best-total hand)
  (find-best-total (get-totals hand 0)))

(best-total '(ad 8s))
(best-total '(ad 8s 5h))
(best-total '(ad as 9h))
(best-total '(jc qd 1s ad))


; Ex. 2
(define (stop-at-17 hand next-card)
  (let ((cards (se hand next-card)))
    (< (best-total cards) 17)))
(stop-at-17 '(ad 8s 5h) '2d)
(stop-at-17 '(ad 8s 5h) '3d)

; Ex. 3
(define (play-n strategy n)
  (define (iter count sum)
    (if (> count n)
        sum
        (iter (+ count 1) (+ sum (twenty-one strategy)))))
  (trace iter)
  (iter 1 0))
(play-n stop-at-17 10)