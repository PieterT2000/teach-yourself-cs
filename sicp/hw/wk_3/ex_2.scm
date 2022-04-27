#lang planet dyoo/simply-scheme:2
; ----------------------------
; Ex. 2
; ----------------------------

(define (sum-of-factors n)
  (define (iter i factor-sum)
    (cond ((= i n) factor-sum)
          ((= (remainder n i) 0) (iter (+ i 1) (+ factor-sum i)))
          (else (iter (+ i 1) factor-sum))))
  (iter 1 0))
(define (next-perf n)
  (if (= (sum-of-factors n) n)
      n
      (next-perf (+ n 1))))
(next-perf 29)

; ----------------------------
; Ex. 4
; ----------------------------

; b^n = product * b^counter 