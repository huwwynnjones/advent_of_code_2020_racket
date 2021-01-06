#lang racket

(require rackunit)

(define (string->op-pair str)
  (let ([str-list (string-split str)])
    (cons (car str-list) (string->number (cadr str-list)))))


; Tests
(test-case
    "Read string into op pair"
  (check-equal? (string->op-pair "nop +0") '("nop" . 0)))
