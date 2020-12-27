#lang racket

(require rackunit)

(define (split-text text)
  '("light red" (list (list 1 "bright white") (list 2 "muted yellow"))))



;Tests
(check-equal? (split-text
               "light red bags contain 1 bright white bag, 2 muted yellow bags."
               )
              '("light red" (list (list 1 "bright white")
                                  (list 2 "muted yellow"))))
