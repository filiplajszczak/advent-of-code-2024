#!/usr/bin/guile -s
!#

(use-modules ((ice-9 regex))
             ((srfi srfi-64) #:select (test-begin
                                       test-end
                                       test-equal))
             ((f) #:select (read-lines))
             ((algorithms) #:select (flatten
                                     product
                                     sum)))

(define (input filename)
   (read-lines filename))

(define (find-all-muls str)
  (map match:substring (list-matches "mul\\([0-9]+,[0-9]+\\)" str)))

(define (find-all-muls-and-dos str)
  (map match:substring (list-matches "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)" str)))

(define (parse-mul str)
  (map string->number (map match:substring (list-matches "[0-9]+" str))))

(define (interpret instructions)
  (define (do? str) (string=? str "do()"))
  (define (dont? str) (string=? str "don't()"))
  (define (do-mul acc remaining)
    (cond [(null? remaining) acc]
          [(dont? (car remaining)) (dont-mul acc (cdr remaining))]
          [(do? (car remaining)) (do-mul acc (cdr remaining))]
          [else (do-mul (cons (product (parse-mul (car remaining))) acc) (cdr remaining))]))
  (define (dont-mul acc remaining)
    (cond [(null? remaining) acc]
          [(dont? (car remaining)) (dont-mul acc (cdr remaining))]
          [(do? (car remaining)) (do-mul acc (cdr remaining))]
          [else (dont-mul acc (cdr remaining))]))
  (do-mul '() instructions))

(define (part-1 filename)
  (sum (map product (map parse-mul (flatten (map find-all-muls (input filename)))))))

(define (part-2 filename)
  (sum (interpret (flatten  (map find-all-muls-and-dos (input filename))))))


;; Part 1
(display (part-1 "day03.in"))
(newline)
;; Part 2
(display (part-2 "day03.in"))
(newline)

;; Tests
(test-begin "example")

(test-equal "Test part 1" 161 (part-1 "day03-example-1.in"))
(test-equal "Test part 2" 48 (part-2 "day03-example-2.in"))

(test-end "example")
