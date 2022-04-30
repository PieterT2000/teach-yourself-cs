#lang planet dyoo/simply-scheme:2
; ----------------------------
; Ex. 2
; ----------------------------

(define (substitute l old-w new-w)
  (define (iter item)
    (cond ((list? item) (map iter item))
          ((word? item) (if (equal? item old-w) new-w item))))
  (iter l))

(substitute '((lead guitar) (bass guitar) (rhythm guitar) drums) 'guitar 'axe)