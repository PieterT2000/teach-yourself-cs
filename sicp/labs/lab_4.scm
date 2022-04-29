#lang planet dyoo/simply-scheme:2
; Ex. 3,4
(define (make-rational num den)
  (cons num den))
(define (numerator rat)
  (car rat))
(define (denominator rat)
  (cdr rat))
(define (*rat a b)
  (make-rational (* (numerator a) (numerator b))
                 (* (denominator a) (denominator b))))
(define (print-rat rat)
  (word (numerator rat) '/ (denominator rat)))

; Ex. 5
(define (+rat a b)
  (make-rational (+ (* (numerator a) (denominator b)) (* (numerator b) (denominator a)))
                 (* (denominator a) (denominator b))))
(print-rat (+rat (make-rational 2 3) (make-rational 1 4)))

; Ex.6

; -- 2.2
(define (make-segment p1 p2)
  (cons p1 p2))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

; Midpoint formula
; x: (x1 + x2)/2
; y: (y1 + y2)/2
(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2)
              (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2)))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

(print-point (midpoint-segment (make-segment (make-point 2.0 1.0) (make-point 5 4))))

; -- 2.3

; Representation with horizontal and vertical segment
(define (make-rect l1 l2)
  (cons l1 l2))
(define (horiz-side rect)
  (car rect))
(define (vert-side rect)
  (cdr rect))

; Assuming rectangels are at 0 degrees angle on x-axis
(define (distance segment)
  (abs (let ((y2 (y-point (end-segment segment))) (y1 (y-point (start-segment segment))))
    (if (= y2 y1)
      (- (x-point (end-segment segment)) (x-point (start-segment segment)))
      (- y2 y1)))))
(define (perimeter rect)
  (* (+ (distance (horiz-side rect)) (distance (vert-side rect))) 2))
(define (area rect)
  (* (distance (horiz-side rect)) (distance (vert-side rect))))

(define rect1 (make-rect (make-segment (make-point 1 4) (make-point 5 4)) (make-segment (make-point 5 4) (make-point 5 1))))
(perimeter rect1)
(area rect1)

; Representation with endpoints of diagonal
(set! make-rect
      (lambda (p1 p2)
        (make-segment p1 p2)))
(set! horiz-side
      (lambda (rect)
        (let ((y (y-point (start-segment rect))))
          (make-segment (make-point (x-point (start-segment rect)) y) (make-point (x-point (end-segment rect)) y)))))
(set! vert-side
      (lambda (rect)
        (let ((x (x-point (end-segment rect))))
          (make-segment (make-point x (y-point (start-segment rect))) (make-point x (y-point (end-segment rect)))))))
(define rect2 (make-rect (make-point 1 4) (make-point 5 1)))
(perimeter rect2)
(area rect2)

; -- 2.4
(define (cons-2 x y)
  (lambda (m) (m x y)))
(define (car-2 z)
  (z (lambda (p q) p)))
(define (cdr-2 z)
  (z (lambda (p q) q)))
(define pair (cons-2 2 3))
(car-2 pair)
(cdr-2 pair)

; Ex. 8 (-- 2.18)
(define (reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l)) (list (car l)))))
(reverse (list 1 4 9 16 25))