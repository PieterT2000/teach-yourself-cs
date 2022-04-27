#lang planet dyoo/simply-scheme:2

; Pigl program
(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))
(define (pl-done? wd)
  (vowel? (first wd)))
(define (vowel? letter)
  (member? letter '(a e i o u)))

; Plural program
(define (plural wd)
  (if (and (equal? (last wd) 'y) (not (vowel? (last (bl wd)))))
      (word (bl wd) 'ies)
      (word wd 's)))
(plural 'boy)
(plural 'sky)

; Sum program
(define (square x)
  (* x x))
(define (sum-of-two-largest a b c)
  (define smallest (min a b c))
  (- (+ (square a) (square b) (square c)) (square smallest)))
(sum-of-two-largest 4 2 5) ; -> 41
(sum-of-two-largest 0 3 1) ; -> 10

; Duplication removal program
(define (dupls-removed sen)
  (if (empty? sen)
      '()
      (se (if (member? (first sen) (bf sen)) '() (first sen)) (dupls-removed (bf sen)))))
(dupls-removed '(a b c a e d e b))
(dupls-removed '(a b c ))
(dupls-removed '(a a a a b a a))