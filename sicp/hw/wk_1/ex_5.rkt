#lang planet dyoo/simply-scheme:2

; ----------------
; Hw problem 5
; ----------------

(define (word-ends-in-e? w)
  (equal? (last w) 'e))

(define (ends-e sen)
  (keep word-ends-in-e? sen))

(ends-e '(please put the salami above the blue elephant))