#!/usr/bin/guile -s
!#

(use-modules ((srfi srfi-1) #:select (count))
             ((srfi srfi-64) #:select (test-begin
                                       test-end
                                       test-equal))
             ((f) #:select (read-lines))
             ((algorithms) #:select (all?
                                     any?
                                     increasing?
                                     sliding)))

(define (input filename)
   (read-lines filename))

(define (parse lines)
  (map (λ (l) (map string->number l))
    (map (λ (l) (string-split l #\space)) lines)))

(define (levels-close? report)
  (all? (map (λ (pair) (<= (abs (- (car pair) (cadr pair))) 3))
  (sliding report 2))))

(define (safe? report)
  (and (levels-close? report) (or (increasing? report) (increasing? (reverse report)))))

(define (candidates lst)
  (define (aux lst prefix)
    (if (null? lst)
        '()
        (cons (append prefix (cdr lst))
              (aux (cdr lst) (append prefix (list (car lst)))))))
  (cons lst (aux lst '())))

(define (safe-enough? report)
  (any? (filter safe? (candidates report))))

(define (part-1 filename)
  (length (filter safe?
        (parse (input filename)))))

(define (part-2 filename)
  (length (filter safe-enough?
        (parse (input filename)))))

;; Part 1
(display (part-1 "day02.in"))
(newline)
; Part 2
(display (part-2 "day02.in"))
(newline)

;; Tests
(test-begin "example")

(test-equal "Test part 1" 2 (part-1 "day02-example.in"))
(test-equal "Test part 2" 4 (part-2 "day02-example.in"))

(test-end "example")
