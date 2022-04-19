#lang planet dyoo/simply-scheme:2

; ----------------------------
; Ex. 2
; ----------------------------
(define (square x) (* x x))
(define (every f sen)
  (if (empty? sen)
      '()
      (se (f (first sen)) (every f (bf sen)))))
(every square '(1 2 3 4))
(every first '(nowhere man))