#!/usr/bin/guile -s
!#

(use-modules ((string transform) #:select (collapse-repeated-chars))
             ((srfi srfi-1) #:select (count zip))
             ((srfi srfi-64) #:select (test-begin
                                       test-end
                                       test-equal))
             ((srfi srfi-197) #:select (chain))
             ((f) #:select (read-lines))
             ((algorithms) #:select (sum zip-with)))

(define (input filename)
   (read-lines filename))

(define (lines-of-pairs->pair-of-lists lines)
  (chain
    lines
    (map (λ (l) (collapse-repeated-chars l)) _)
    (map (λ (l) (string-split l #\space)) _)
    (map (λ (str) (map string->number str)) _)
    (apply zip _)))

(define (count-occurrences elem lst)
  (count (λ (x) (equal? x elem)) lst))

(define (similarity-scores lsts)
  (let ([first (car lsts)]
        [second (cadr lsts)])
    (map (λ (elem) (* elem (count-occurrences elem second))) first)))

(define (part-1 filename)
  (chain
    (input filename)
    (lines-of-pairs->pair-of-lists _)
    (map (λ (l) (sort l <)) _)
    (apply zip-with (λ (a b) (abs (- a b))) _)
    (sum _)))

(define (part-2 filename)
  (chain
    (input filename)
    (lines-of-pairs->pair-of-lists _)
    (similarity-scores _)
    (sum _)))

;; Part 1
(display (part-1 "day01.in"))
(newline)
; Part 2
(display (part-2 "day01.in"))
(newline)

;; Tests
(test-begin "example")

(test-equal "Test part 1" 11 (part-1 "day01-example.in"))
(test-equal "Test part 2" 31 (part-2 "day01-example.in"))

(test-end "example")
