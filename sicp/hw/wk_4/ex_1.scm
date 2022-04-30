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

; 2.8
(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
                 (- (upper-bound a) (lower-bound b))))
(define intv-1 (make-interval 2 4))
(define intv-2 (make-interval 3 5))
;(sub-interval intv-1 intv-2)

; 2.10
(define (mul-interval x y)
  "multiplication done")

(define (div-interval x y)
  (if (or (= (lower-bound y) 0) (= (upper-bound y) 0))
      (error "Lower of upper bound of divisor can't be zero")
       (mul-interval x
                     (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))
;(div-interval (make-interval 0 5) (make-interval 0 4))

; 2.12
(define (make-center-percent c p)
  (let ((w (* c (/ p 100))))
    (make-interval (- c w) (+ c w))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (* (/ (width i) (center i)) 100))
;(percent (make-center-percent 5 10))

; 2.17
(define (last-pair l)
  (if (= (count l) 1)
      (car l)
      (last-pair (cdr l))))
;(last-pair (list 23 72 149 34))

; 2.20
(define (same-parity . l)
  (if (even? (car l))
      (filter even? l)
      (filter odd? l)))
;(same-parity 1 2 3 4 5 6 7)
;(same-parity 2 3 4 5 6 7)

; 2.22
; Cons is great at appending things at the front but with the unfortunate result that things appear in reverse order.
; Interchanging the arguments results in a deep list as we wrap answer which is a list inside a cons. So we make pairs of lists.
; The solution would be to use append with wrapping the next item in a one-item list. Append takes two lists and instead of returning a list of lists, flattens them out.
(define (square x) (* x x))
(define (square-list items)
  (define (iter things answer)
    (if (empty? things)
        answer
        (iter (cdr things)
              (append answer
                    (list (square (car things)))))))
  (iter items '()))
(square-list (list 1 2 3 4))

; 2.23
(define (for-each f l)
  (if (empty? l)
      #t
      (begin
        (f (car l))
        (for-each f (cdr l)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))