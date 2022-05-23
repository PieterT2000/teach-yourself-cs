#lang sicp
(#%require sicp-pict)

; ----------------------------
; Programming Project 2: Picture Language
; ----------------------------

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

; Ex. 2.48
(define (make-segment start-vect end-vect)
  (cons start-vect end-vect))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

; Ex. 2.49
; a.
(define outline-painter
  (segments->painter (list
                      (segment (vect 0 0) (vect 1 0))
                      (segment (vect 1 0) (vect 1 1))
                      (segment (vect 1 1) (vect 0 1))
                      (segment (vect 0 1) (vect 0 0)))))
(define x-painter
  (segments->painter (list
                      (segment (vect 0 0) (vect 1 1))
                      (segment (vect 0 1) (vect 1 0)))))
(define diamond-painter
  (segments->painter (list
                      (segment (vect 0 0.5) (vect 0.5 0))
                      (segment (vect 0.5 0) (vect 1 0.5))
                      (segment (vect 1 0.5) (vect 0.5 1))
                      (segment (vect 0.5 1) (vect 0 0.5)))))
(define wave
  (segments->painter (list
                      (segment (vect 0.006 0.840) (vect 0.155 0.591))
                      (segment (vect 0.006 0.635) (vect 0.155 0.392))
                      (segment (vect 0.304 0.646) (vect 0.155 0.591))
                      (segment (vect 0.298 0.591) (vect 0.155 0.392))
                      (segment (vect 0.304 0.646) (vect 0.403 0.646))
                      (segment (vect 0.298 0.591) (vect 0.354 0.492))
                      (segment (vect 0.403 0.646) (vect 0.348 0.845))
                      (segment (vect 0.354 0.492) (vect 0.249 0.000))
                      (segment (vect 0.403 0.000) (vect 0.502 0.293))
                      (segment (vect 0.502 0.293) (vect 0.602 0.000))
                      (segment (vect 0.348 0.845) (vect 0.403 0.999))
                      (segment (vect 0.602 0.999) (vect 0.652 0.845))
                      (segment (vect 0.652 0.845) (vect 0.602 0.646))
                      (segment (vect 0.602 0.646) (vect 0.751 0.646))
                      (segment (vect 0.751 0.646) (vect 0.999 0.343))
                      (segment (vect 0.751 0.000) (vect 0.597 0.442))
                      (segment (vect 0.597 0.442) (vect 0.999 0.144)))))
;(paint outline-painter)
;(paint x-painter)
;(paint diamond-painter)
(paint wave)

; Ex. 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (vect 1 0)
                     (vect 0 0)
                     (vect 1 1)))
(define (rotate180 painter)
  (rotate90 (rotate90 painter)))
(define (rotate270 painter)
  (rotate90 (rotate180 painter)))

; Ex. 2.51
(define (below p1 p2)
  (let ((split-point (vect 0 0.5)))
    (let ((paint-top
           (transform-painter p2
                              split-point
                              (vect 1 0.5)
                              (vect 0 1)))
          (paint-bottom
           (transform-painter p1
                              (vect 0 0)
                              (vect 1 0)
                              split-point)))
      (lambda (f)
        (paint-top f)
        (paint-bottom f)))))

(define (below-2 p1 p2)
  (rotate90 (beside
             (rotate270 p1)
             (rotate270 p2)
   )))
; Ex. 2.44
(define (up-split-2 painter n)
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

;(paint (below-2 einstein einstein))

; Ex. 2.52
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))
(paint (corner-split einstein 3))
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
(paint (square-limit (flip-horiz einstein) 4))
(paint (square-limit wave 4))