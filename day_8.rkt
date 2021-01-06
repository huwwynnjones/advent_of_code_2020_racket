#lang racket

(require rackunit)

(define (string->op-pair str)
  (let ([str-list (string-split str)])
    (cons (string->op-symbol (car str-list))
          (string->number (cadr str-list)))))

(define (string->op-symbol str)
  (match str
    ["nop" 'nop]))


; Tests
(test-case
    "Read string into op pair"
  (check-equal? (string->op-pair "nop +0") '(nop . 0)))
(test-case
    "Load program into vector"
  (check-equal? (load-input-file "day_8_test.rkt")
                (`(
                   ,'(nop 0)
                   ,'(acc 1)))))
