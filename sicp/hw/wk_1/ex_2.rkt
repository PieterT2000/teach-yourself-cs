#lang planet dyoo/simply-scheme:2
#| pigl
(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))
|#

; ----------------
; Hw problem 2
; ----------------

(define (square x)
  (* x x))

(define (squares numbers)
  (if (null? numbers)
      numbers
      (sentence (square (first numbers)) (squares (bf numbers)))))

(squares '(2 3 4 5))