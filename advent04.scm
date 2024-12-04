#!/usr/bin/guile -s
!#

(use-modules
  ((ice-9 regex))
  ((srfi srfi-1) #:select (concatenate lset= zip))
  ((srfi srfi-64) #:select (test-begin test-end test-equal))
  ((f) #:select (read-lines))
  ((algorithms) #:select (flatten sum)))

(define (input filename)
  (map string->list (read-lines filename)))

(define (apply-n-times proc x n)
  (if (<= n 0)
      x
      (apply-n-times proc (proc x) (- n 1))))

(define (transpose lst-of-lsts)
  (apply zip lst-of-lsts))

(define (rotate lst-of-lsts)
  (map reverse (transpose lst-of-lsts)))

(define (get-diagonals lst-of-lsts)
  (let ([num-rows (length lst-of-lsts)] 
        [num-cols (length (car lst-of-lsts))])
    (define (get-primary start-row start-col)
      (let aux ((row start-row) (col start-col) (diagonal '()))
        (if (or (>= row num-rows) (>= col num-cols))
            (reverse diagonal)
            (aux (+ row 1) (+ col 1)
                  (cons (list-ref (list-ref lst-of-lsts row) col) diagonal)))))
    (define (get-secondary start-row start-col)
      (let aux ((row start-row) (col start-col) (diagonal '()))
        (if (or (>= row num-rows) (< col 0))
            (reverse diagonal)
            (aux (+ row 1) (- col 1)
                  (cons (list-ref (list-ref lst-of-lsts row) col) diagonal)))))
    (append
      (map (λ (col) (get-primary 0 col)) (iota num-cols))
      (map (λ (row) (get-primary row 0)) (cdr (iota num-rows)))
      (map (λ (col) (get-secondary 0 col)) (iota num-cols))
      (map (λ (row) (get-secondary row (- num-cols 1))) (cdr (iota num-rows))))))

(define (count-xmas lst)
  (length
    (map match:substring (list-matches "XMAS" (list->string lst)))))

(define (rotations lst-of-lsts)
  (map (λ (n) (apply-n-times rotate lst-of-lsts n))
       (iota 4)))

(define (part-1 filename)
  (let*
    ([data (input filename)]
     [diagonals (get-diagonals data)])
    (sum
      (map count-xmas
        (append
          (concatenate (rotations data))
          diagonals
          (map reverse diagonals))))))

(define (get-element list-of-lists row col)
  (let ((inner-list (list-ref list-of-lists row)))
    (list-ref inner-list col)))

(define (check-neighbors list-of-lists row col)
  (let ([upper-left (get-element list-of-lists (- row 1) (- col 1))]
        [upper-right (get-element list-of-lists (- row 1) (+ col 1))]
        [lower-left (get-element list-of-lists (+ row 1) (- col 1))]
        [lower-right (get-element list-of-lists (+ row 1) (+ col 1))]
        [ms-elements (list #\M #\S)])
    (if
      (and
        (lset= char=? (list upper-left lower-right) ms-elements)
        (lset= char=? (list upper-right lower-left) ms-elements))
      1
      0)))

(define (check-for-xmas list-of-lists row col)
  (let ([height (length list-of-lists)]
         [width (length (car list-of-lists))]
         [char (get-element list-of-lists row col)])
    (if (or
          (= row 0)
          (= col 0)
          (= row (- height 1))
          (= col (- width 1))
          (not (char=? char #\A)))
        0
        (check-neighbors list-of-lists row col))))

(define (part-2 filename)
  (let ([data (input filename)])
    (sum
      (flatten
        (map
          (λ (row) (map
                     (λ (col) (check-for-xmas data row col))
                     (iota (length (car data)))))
          (iota (length data)))))))

;; Part 1
(display (part-1 "day04.in"))
(newline)
;; Part 2
(display (part-2 "day04.in"))
(newline)

;; Tests
(test-begin "example")

(test-equal "Test part 1" 18 (part-1 "day04-example.in"))
(test-equal "Test part 2" 9 (part-2 "day04-example.in"))

(test-end "example")
