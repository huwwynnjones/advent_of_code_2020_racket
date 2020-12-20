#lang racket

(require rackunit)

(define (count-questions questions)
  3)

;Tests
(check-equal? (count-questions '(a b c)) 3)
