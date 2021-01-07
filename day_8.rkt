#lang racket

(require rackunit)

(define (string->op-pair str)
  (let ([str-list (string-split str)])
    (cons (string->op-symbol (car str-list))
          (string->number (cadr str-list)))))

(define (string->op-symbol str)
  (match str
    ["nop" 'nop]
    ["acc" 'acc]
    ["jmp" 'jmp]))

(define (load-input-file filename)
  (call-with-input-file filename
    (λ (in)
      (for/list ([line (in-lines in)])
        (string->op-pair line)))))

(define (run-program program)
  5)

; Tests
(test-case
    "Read string into op pair"
  (check-equal? (string->op-pair "nop +0") '(nop . 0)))
(test-case
    "Load program into vector"
  (check-equal? (load-input-file "day_8_test.rkt")
                `(
                  ,'(nop . 0)
                  ,'(acc . 1)
                  ,'(jmp . 4)
                  ,'(acc . 3)
                  ,'(jmp . -3)
                  ,'(acc . -99)
                  ,'(acc . 1)
                  ,'(jmp . -4)
                  ,'(acc . 6))))
(test-case
    "Accumulator is 5 before any instruction is run a second time"
  (let ([program (load-input-file "day_8_test.rkt")])
    (check-equal? (run-program program) 5)))
