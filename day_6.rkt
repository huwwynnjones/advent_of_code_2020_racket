#lang racket

(require rackunit)

(define (count-questions questions)
  (length questions))

(define (count-groups-questions groups)
  (count-questions
   (remove-duplicates
    (flatten groups))))

;Tests
(check-equal? (count-questions '("a" "b" "c")) 3)
(check-equal? (count-groups-questions '(("a" "b")
                                        ("a" "c"))) 3)
