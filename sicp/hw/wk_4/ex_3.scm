#lang planet dyoo/simply-scheme:2
; ----------------------------
; Ex. 3
; ----------------------------

; Works on 1-indexed sentences. Returns 0 if not found.
(define (index-of list-val list)
  (define (iter count)
    (cond ((empty? list) 0)
          ((equal? list-val (item count list)) count)
          (else (iter (+ count 1)))))
  (iter 1))

(define (substitute-2 l old-w-list new-w-list)
  (define (iter list-val)
    (cond ((list? list-val) (map iter list-val))
          (else (if (member? list-val old-w-list)
                    (item (index-of list-val old-w-list) new-w-list)
                    list-val))))

  (iter l))

(substitute-2 '((4 calling birds) (3 french hens) (2 turtle doves)) '(1 2 3 4) '(one two three four))