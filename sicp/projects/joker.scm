#lang planet dyoo/simply-scheme:2

; ----------------------------
; Programming Project 1: Twenty-One (with joker)
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

(define joker 'XX)

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C) joker joker) )

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
(define (assert a b)
  (if (equal? a b)
      "True"
      "False"))

(define (rank card)
  (bl card))

(define (get-suit card)
  (last card))

(define (get-totals sent total)
  (define (joker-val-iter count)
    (if (> count 11)
        '()
        (se (get-totals (bf sent) (+ total count)) (joker-val-iter (+ count 1)))))
  (if (empty? sent)
      (se total)
      (let ((val (rank (first sent))))
        (cond ((number? val) (get-totals (bf sent) (+ total val)))
              ((member? val 'aA) (se (get-totals (bf sent) (+ total 11)) (get-totals (bf sent) (+ total 1))))
              ((member? val 'xX) (joker-val-iter 1))
              ((member? val 'jqkJQK) (get-totals (bf sent) (+ total 10)))
              ))))

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

; JOKER TEST
(display "JOKER TEST - (get-totals '(XX) 0) => '(1 2 3 4 5 6 7 8 9 10 11): ")
(assert (get-totals (se joker) 0) '(1 2 3 4 5 6 7 8 9 10 11))
(display "JOKER TEST - (count (get-totals '(XX XX) 0)) => 121: ")
(assert (count (get-totals (se joker joker) 0)) 121)
(display "JOKER TEST - (best-total '(XX AS 3D)) => 21: ")
(assert (best-total (se joker 'AS '3D)) 21)

; Ex. 2
(define (stop-at-17 hand next-card)
  (let ((cards hand))
    (< (best-total cards) 17)))
;(stop-at-17 '(ad 8s 5h) '2d)
;(stop-at-17 '(ad 8s 5h) '3d)

; Ex. 3
(define (play-n strategy n)
  (define (iter count sum)
    (if (> count n)
        sum
        (iter (+ count 1) (+ sum (twenty-one strategy)))))
  (iter 1 0))

; Ex. 4
(define (dealer-sensitive hand next-card)
  (define (first-cond val)
    (if (number? val)
        (and (> val 6) (< val 11))
        (member? val 'ajqkAJQK)))
  (define (second-cond val)
    (if (number? val)
        (and (> val 1) (< val 7))
        #f))
  (let ((total (best-total hand)) (val (rank next-card)))
    (or (and (first-cond val) (< total 17))
        (and (second-cond val) (< total 12)))))

; Ex. 5
(define (stop-at n)
  (lambda (hand next-card)
      (< (best-total hand) n)))
; Ex. 6
(define (valentine hand next-card)
  ((suit-strategy 'h (stop-at 19) (stop-at 17)) hand next-card))
;(valentine '(ad as 9h) '10h)

; Ex. 7
(define (suit-strategy suit pos-strategy neg-strategy)
  (lambda (hand next-card)
    (let ((suits (every get-suit hand)))
      (if (member? suit suits)
          (pos-strategy hand next-card)
          (neg-strategy hand next-card)))))
(display "valentine 20 trials: ")
(play-n valentine 20)

; Ex. 8
(define (majority strat-1 strat-2 strat-3)
  (define (majority-strategy hand next-card)
    (let ((r1 (strat-1 hand next-card)) (r2 (strat-2 hand next-card)) (r3 (strat-3 hand next-card)))
      (or (and r1 r2) (and r1 r3) (and r2 r3))))
  ;(trace majority-strategy)
  majority-strategy)
(display "majority 10 trials: ")
(play-n (majority valentine dealer-sensitive (stop-at 10)) 10)

; Ex. 9
(define (reckless strategy)
  (lambda (hand next-card)
    (strategy (bl hand) next-card)))
