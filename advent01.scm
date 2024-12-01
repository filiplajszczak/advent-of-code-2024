#!/usr/bin/guile -s
!#

(use-modules ((ice-9 string-fun) #:select (string-replace-substring))
             ((srfi srfi-1) #:select (count))
             ((srfi srfi-64) #:select (test-begin
                                       test-end
                                       test-equal))
             ((f) #:select (read-lines))
             ((algorithms) #:select (sum zip-with)))

(define (input filename)
   (read-lines filename))

(define (lines-of-pairs->pair-of-lists lines)
  (apply map list
    (map (λ (str) (map string->number str))
      (map (λ (l) (string-split l #\space))
        (map (λ (l) (string-replace-substring l "   " " ")) lines)))))

(define (count-occurrences elem lst)
  (count (λ (x) (equal? x elem)) lst))

(define (similarity-scores lsts)
  (let ([first (car lsts)]
        [second (cadr lsts)])
    (map (λ (elem) (* elem (count-occurrences elem second))) first)))

(define (part-1 filename)
  (sum
    (apply zip-with (λ (a b) (abs (- a b)))
      (map (λ (l) (sort l <))
        (lines-of-pairs->pair-of-lists (input filename))))))

(define (part-2 filename)
  (sum
    (similarity-scores
      (lines-of-pairs->pair-of-lists (input filename)))))

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
