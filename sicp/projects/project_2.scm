#lang planet dyoo/simply-scheme:2

; ----------------------------
; Programming Project 2: Picture Language
; ----------------------------

; Ex. 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

; Ex. 2.45
(define (split op-1 op-2)
  (define (split-painter painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-painter painter (- n 1))))
          (op-1 painter (op-2 smaller smaller)))))
  split-painter)
(define up-split (split below beside))
(define right-split (split beside below))

; Ex. 2.46
(define (make-vect x-cor y-cor)
  (cons x-cor y-cor))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cdr vect))

(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect vect scalar)
  (make-vect
   (* (xcor-vect vect) scalar)
   (* (ycor-vect vect) scalar)))

; Ex. 2.47

; frame implementation is (list x y z)
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

; frame implementation is (cons x (cons y z))
; Selector origin-frame and edge1-frame can be reused
(define (edge2-frame-2 frame)
  (cddr frame))