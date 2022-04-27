#lang planet dyoo/simply-scheme:2

; ----------------
; Hw problem 3
; ----------------

(define (check-word w)
  (cond ((or (equal? w 'I) (equal? w 'me)) 'you)
        ((equal? w 'you) 'me)
        ((equal? w 'You) 'I)
        (else w)))

(define (switch s)
  (if (null? s)
      s
      (sentence (check-word (first s)) (switch (bf s)))))

(switch '(You told me that I should wake you up))