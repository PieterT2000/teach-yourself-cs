#lang planet dyoo/simply-scheme:2
; ----------------------------
; Ex. 1
; ----------------------------

; 1.16
(define (square x) (* x x))
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
;(trace fast-expt)
;(fast-expt 2 6)

(define (expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (expt-iter (square b) (/ n 2) a))
        (else (expt-iter b (- n 1) (* a b)))))
(define (expt b n)
  (expt-iter b n 1))

;(trace expt-iter)
(expt 2 6)

; 1.35
(define (close-enough? v1 v2 tolerance)
  (< (abs (- v1 v2)) tolerance))
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next tolerance)
          guess
          (try next))))
  (try first-guess))
(define (test-fn x)
  (+ 1 (/ 1 x)))
(define golden-ratio (fixed-point test-fn 1.0))
(display golden-ratio)
(newline)

; 1.37
(define (cont-frac n d k)
  (define (iter n d i)
    (if (= i k)
      1
      (/ (n (+ i 1)) (+ (d (+ i 1)) (iter n d (+ i 1))))))
  (iter n d 0))

(define one (lambda (i) 1.0))
(cont-frac one one 10)
(define (find-k-term k)
  (if (close-enough? (- golden-ratio 1) (cont-frac one one k) 0.00001)
      k
      (find-k-term (+ k 1))))

;(trace find-k-term)
(find-k-term 1) ; => 12

; 1.38
(define (get-d i)
  (define (iter count sum item)
    (cond ((= count i) item)
          ((< count 2) (iter (+ count 1) (+ sum 1) (+ count 1)))
          ((= (remainder (- count 1) 3) 0) (iter (+ count 1) sum sum))
          (else (iter (+ count 1) (+ sum 1) 1))))
  (iter 0 0 0))

(define (find-e k)
  (cont-frac one get-d k))
(find-e 10)