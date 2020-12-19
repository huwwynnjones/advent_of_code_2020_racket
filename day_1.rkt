#lang racket

(require rackunit)

(provide find-two-that-sum-2020
         find-three-that-sum-2020
         load-input-file)

(define (find-two-that-sum-2020 input)
  (for*/first ([i input]
               [j input]
               #:when (= (+ i j) 2020))
    (list i j)))


(define (find-three-that-sum-2020 input)
  (for*/first ([i input]
               [j input]
               [k input]
               #:when (= (+ i j k) 2020))
    (list i j k)))

(define (load-input-file file-name)
  (call-with-input-file file-name
    (λ (in)
      (for/list ([l (in-lines in)])
        (string->number l)))))


(check-equal? (find-two-that-sum-2020 '(1721 979 366 299 675 1456)) '(1721 299))
