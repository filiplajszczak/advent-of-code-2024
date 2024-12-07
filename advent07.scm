#!/usr/bin/guile -s
!#

(use-modules ((ice-9 string-fun) #:select (string-replace-substring))
  ((srfi srfi-1) #:select (filter-map any))
             ((srfi srfi-64) #:select (test-begin
                                       test-end
                                       test-equal))
             ((f) #:select (read-lines))
             ((algorithms) #:select (sum)))

(define (input filename)
   (read-lines filename))

(define (permutations n ops)
  (if (= n 0)
      '(())
      (let ((sub-perms (permutations (- n 1) ops)))
        (apply append
               (map (λ (e) (map (λ (perm) (cons e perm)) sub-perms)) ops)))))

(define (perm-ok? perm total numbers)
    (define (aux operations acc left)
      (if (null? operations)
          #f
          (let* ([operation (car operations)]
                 [result (operation acc (car left))])
              (if (and (null? (cdr operations)) (= total result))
                  #t
                  (aux (cdr operations) result (cdr left))))))
    (aux perm (car numbers) (cdr numbers)))

(define (ok? line ops)
  (let* ([total (car line)]
         [numbers (cdr line)]
         [perms (permutations (- (length numbers) 1) ops)])
    (any (λ (perm) (perm-ok? perm total numbers)) perms)))

(define (|| a b)
  (+ (* a (expt 10 (1+ (floor (log10 b))))) b))

(define (solve ops filename)
  (let* ([data (map (λ (line) (map string->number (string-split (string-replace-substring line ":" "") #\space))) (input filename))])
    (sum (filter-map (λ (line) (and (ok? line ops) (car line))) data))))

(define (part-1 filename)
  (solve (list + *) filename))
(define (part-2 filename)
  (solve (list + * ||) filename))

;; Part 1
(display (part-1 "day07.in"))
(newline)
;; Part 2
(display (part-2 "day07.in"))
(newline)

;; Tests
(test-begin "example")

(test-equal "Test part 1" 3749 (part-1 "day07-example.in"))
(test-equal "Test part 2" 11387 (part-2 "day07-example.in"))

(test-end "example")
