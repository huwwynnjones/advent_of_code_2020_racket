#lang racket

(require rackunit)


(define (list-gaps lst)
  (if (= (length lst) 1)
      '()
      (flatten
       (list
        (- (second lst) (first lst))
        (list-gaps (rest lst))))))

(define (count-occurences lst nmb)
  (count
   (λ (i)
     (= i nmb))
   lst))

(define (count-jolt-differences lst)
  (cons
   (+ (count-occurences lst 1) 1)
   (+ (count-occurences lst 3) 1)))

(define (find-jolt-differences lst)
  (count-jolt-differences (list-gaps (sort lst <))))

(define (adapter-calculation lst)
  (let ([jolts (find-jolt-differences lst)])
    (* (car jolts) (cdr jolts))))

(define (load-input-file filename)
  (call-with-input-file filename
    (λ (in)
      (for/list ([line (in-lines in)])
        (string->number line)))))

; Tests
(define simple-test '(16 10 15 5 1 11 7 19 6 12 4))
(define large-test '(28 33 18 42 31 14 46 20 48 47
                        24 23 49 45 19 38 39 11 1 32
                        25 35 8 17 7 9 4 2 34 10 3))

(test-case
 "Check that volt differences are counted correctly"
 (check-equal? (find-jolt-differences simple-test) '(7 . 5))
 (check-equal? (find-jolt-differences large-test) '(22 . 10)))
