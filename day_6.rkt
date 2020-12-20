#lang racket

(require rackunit)

(define (count-questions questions)
  (length questions))

(define (count-groups-questions groups)
  (count-questions
   (remove-duplicates
    (flatten groups))))

(define (load-input-file file-name)
  (call-with-input-file file-name
    (λ (in)
      (for/list ([line (in-lines in)]
                 #:when (not (= (string-length line) 0)))
        (string->list line)))))

;Tests
(check-equal? (count-questions '("a" "b" "c")) 3)
(check-equal? (count-groups-questions '(("a" "b")
                                        ("a" "c"))) 3)
(check-equal? (load-input-file
               "day_6_test.txt")
              `(,(string->list "lqhksfnerg")
                ,(string->list "negsc")))
