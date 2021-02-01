#lang racket

(require rackunit)

(provide run-program-sum
         run-multiple-programs
         load-input-file
         create-alternate-programs)

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
  (for/fold ([sum 0]
             [ptr 0]
             [used-instructions null])
            ([op-pair program])
    #:break (or (member ptr used-instructions) (= ptr (length program)))
    (let* ([current (list-ref program ptr)]
           [op (car current)]
           [value (cdr current)])
      (match op
        ['nop (values sum (+ ptr 1) (append used-instructions (list ptr)))]
        ['jmp (values sum (+ ptr value) (append used-instructions (list ptr)))]
        ['acc (values (+ sum value) (+ ptr 1) (append used-instructions (list ptr)))]))))

(define (run-program-sum program)
  (let-values ([(sum ptr used-instructions) (run-program program)])
    (if (= ptr (length program))
        (cons sum #t)
        (cons sum #f))))

(define (run-multiple-programs programs)
  (for/first ([program programs]
              #:when (cdr (run-program-sum program)))
    (car (run-program-sum program))))
    
(define (nop-and-jmp-positions program)
  (for/fold ([idx 0]
             [positions null]
             #:result positions)
            ([op-pair program])
    (let ([op (car op-pair)])
      (if (or
             (equal? op 'nop)
             (equal? op 'jmp))
          (values (+ idx 1) (append positions (list idx)))
          (values (+ idx 1) positions)))))

(define (create-alternate-programs program)
  (let ([positions (nop-and-jmp-positions program)])
    (for/list ([idx positions])
      (list-update program idx change-pair))))

(define (change-pair op-pair)
  (let ([op (car op-pair)])
    (match op
      ['nop (cons 'jmp (cdr op-pair))]
      ['jmp (cons 'nop (cdr op-pair))]
      [_ op-pair])))
    
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
    (check-equal? (run-program-sum program) '(5 . #f))))

(define correct-input `(
                  ,'(nop . 0)
                  ,'(acc . 1)
                  ,'(jmp . 4)
                  ,'(acc . 3)
                  ,'(jmp . -3)
                  ,'(acc . -99)
                  ,'(acc . 1)
                  ,'(nop . -4)
                  ,'(acc . 6)))
(test-case
    "Check run-program will terminate with the correct input"
  (check-equal? (run-program-sum correct-input) '(8 . #t)))
