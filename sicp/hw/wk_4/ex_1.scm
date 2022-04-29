#lang planet dyoo/simply-scheme:2
; ----------------------------
; Ex. 1
; ----------------------------

; 2.7
(define (make-interval a b) (cons a b))

(define (lower-bound x)
  (car x))
(define (upper-bound x)
  (cdr x))

(define intv (make-interval 3 9))
;(lower-bound intv)
;(upper-bound intv)

(define l (list 1 2 3 4))
;l
;(cons 5 l)