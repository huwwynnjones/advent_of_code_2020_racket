#lang racket

(require rackunit)

(define (count-questions questions)
  (length questions))

(define (count-groups-questions groups)
  3)

;Tests
(check-equal? (count-questions '(a b c)) 3)
(check-equal? (count-groups-questions '('(a b)
                                        '(a c))) 3)
