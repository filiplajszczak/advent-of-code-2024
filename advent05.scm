#!/usr/bin/guile -s
!#

(use-modules ((srfi srfi-1) #:select (list-index take drop lset<= lset-difference))
             ((srfi srfi-64) #:select (test-begin
                                       test-end
                                       test-equal))
             ((f) #:select (read-lines))
             ((algorithms) #:select (sum all?)))

(define (input filename)
   (read-lines filename))

(define (strings->numbers lst separator)
    (map (λ (elem) (map string->number (string-split elem separator))) lst))

(define (check-rule rule update)
  (let ([before (car rule)]
        [after (cadr rule)])
    (if (not (lset<= eq? rule update))
      #t
      (let aux ((to-check (reverse update)))
        (if (= (car to-check) after)
            (if (member before (cdr to-check)) #t #f)
            (aux (cdr to-check)))))))

(define (check-update update rules)
  (all? (map (λ (rule) (check-rule rule update)) rules)))

(define (get-middle lst)
  (list-ref lst (/ (- (length lst) 1) 2)))

(define (fix order-update)
  (filter (λ (elem) (member elem (cadr order-update))) (car order-update)))

(define (rules->order rules update)
  (let ([relevant-rules (filter (λ (rule) (lset<= eq? rule update)) rules)])
    (let aux ((rules relevant-rules) (order '()))
      (if (null? (cdr rules))
        (list  (append (car rules) order) update)
          (let* ([highest (car (lset-difference eq? (map cadr rules) (map car rules)))]
                 [new-order (cons highest order)]
                 [new-rules (filter (λ (rule) (not (= (cadr rule) highest))) rules)])
            (aux new-rules new-order))))))

(define (part-1 filename)
  (let* ([data (input filename)]
         [separator (list-index (lambda (elem) (string=? "" elem)) data)]
         [rules (strings->numbers (take data separator) #\|)]
         [updates (strings->numbers (drop data (+ 1 separator)) #\,)])
    (sum (map get-middle (filter (λ (update) (check-update update rules)) updates)))))

(define (part-2 filename)
  (let* ([data (input filename)]
         [separator (list-index (lambda (elem) (string=? "" elem)) data)]
         [rules (strings->numbers (take data separator) #\|)]
         [updates (strings->numbers (drop data (+ 1 separator)) #\,)]
         [updates-to-fix (filter (λ (update) (not (check-update update rules))) updates)]
         [orders-for-updates (map (λ (update) (rules->order rules update)) updates-to-fix)])
    (sum (map get-middle (map (λ (order-update) (fix order-update)) orders-for-updates)))))

;; Part 1
(display (part-1 "day05.in"))
(newline)
; Part 2
(display (part-2 "day05.in"))
(newline)

;; Tests
(test-begin "example")

(test-equal "Test part 1" 143 (part-1 "day05-example.in"))
(test-equal "Test part 2" 123 (part-2 "day05-example.in"))

(test-end "example")
