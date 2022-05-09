#lang planet dyoo/simply-scheme:2

; ----------------------------
; Ex. 1
; ----------------------------

; -- 2.26
; Inputs are '(1 2 3) and '(4 5 6) 
; a. '(1 2 3 4 5 6) -- append
; b. '((1 2 3) 4 5 6) -- cons
; c. '((1 2 3) (4 5 6)) -- list

; -- 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (let ((left (left-branch mobile)) (right (right-branch mobile)))
    (cond ((number? right) right)
          ((number? left) (total-weight right))
          (else (+ (total-weight left) (total-weight right))))))

(define mobile-1 (make-mobile (make-branch 1 (make-mobile (make-branch 1 12) (make-branch 1 3))) (make-branch 1 (make-mobile (make-branch 1 7) (make-branch 1 8)))))
;(trace total-weight)
(total-weight mobile-1)

(define (balanced? mobile)
  (define (torque branch)
    (if (number? branch)
        branch
        (let ((left (branch-length branch)))
          (cond ((number? left) (* left (torque (branch-structure branch))))
                (else (+ (torque (left-branch branch)) (torque (right-branch branch))))))))
  (equal? (torque (left-branch mobile)) (torque (right-branch mobile))))
(balanced? mobile-1)

; -- 2.30
(define l (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))

(define (square x) (* x x))
(define (square-tree-1 tree)
  (if (empty? tree)
      '()
      (let ((el (car tree)))
        (cons (if (number? el)
                  (square el)
                  (square-tree-1 el))
              (square-tree-1 (cdr tree))))))
(square-tree-1 l)

(define (square-tree-2 tree)
  (if (not (list? tree))
      (square tree)
      (map square-tree-2 tree)))
(square-tree-2 l)

; -- 2.31
(define (tree-map fn tree)
  (if (not (list? tree))
      (fn tree)
      (map (lambda (item) (tree-map fn item)) tree)))
(define (square-tree tree) (tree-map square tree))
(square-tree l)

; -- 2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (i) (cons (car s) i)) rest)))))
;(trace subsets)
(subsets (list 1 2 3))
; The recursive algorithm used here is that the subsets of (x y z) are the subsets of (y z) and the subsets of (y z) prefixed with x. 

; -- 2.36
(define (accumulate-n op seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op (map car seqs))
            (accumulate-n op (map cdr seqs)))))
(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(accumulate-n + s)

; -- 2.37
(define (dot-product v w)
  (accumulate + (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))
(define m-1 '((0 3 5) (5 5 2) (3 5 4)))
(define v-1 '(3 4 3))
;(matrix-*-vector m-1 v-1)

(define (transpose mat)
  (accumulate-n (if (= (count mat) 2) list se) mat))
(transpose m-1)
(transpose '((1 2) (3 4)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r) (map (lambda (c) (dot-product r c)) cols)) m)))
(define m-3 '((1 2 3) (3 4 1) (5 5 3)))
(define n-3 '((3 2 1) (2 1 3) (3 4 1)))
(matrix-*-matrix m-3 n-3)

; -- 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (trace iter)
  (iter initial sequence))
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))
(trace fold-right)
;(fold-left / 1 (list 1 2 3)) -> 1/6
;(fold-right / 1 (list 1 2 3)) -> 3/2
;(fold-right list '() (list 1 2 3)) -> '(1 (2 (3 ())))
;(fold-left list '() (list 1 2 3)) -> '(((() 1) 2) 3)

; -- 2.54
(define (_equal? a b)
  (cond ((or (empty? a ) (empty? b)) #t)
        ((eq? (car a) (car b)) (_equal? (cdr a) (cdr b)))
        (else #f)))
(trace _equal?)
(_equal? '(1 a b 3 ()) '(1 a b 3 ()))