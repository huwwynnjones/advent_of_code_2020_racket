#lang racket

(require rackunit)

(provide count-all-groups-questions
         load-input-file)

(define (count-questions questions)
  (length questions))

(define (convert-to-chars-no-duplicates questions)
  (remove-duplicates
   (flatten
    (for/list ([q questions])
      (string->list q)))))

(define (count-group-questions group)
  (count-questions
   (convert-to-chars-no-duplicates group)))

(define (count-all-groups-questions groups)
  (for/sum ([group groups])
    (count-group-questions group)))

(define (extend old new)
  (foldr cons (list new) old))

(define (split-into-groups items)
  (for/fold ([groups null]
             [group null]
             #:result (extend groups group))
            ([item items])
    (cond
      [(= (string-length item) 0)
       (values (extend groups group) null)]
      [else
       (values groups (append group (list item)))])))

(define (load-input-file file-name)
  (call-with-input-file file-name
    (λ (in)
      (split-into-groups (in-lines in)))))

;Tests
(check-equal? (count-questions '("a" "b" "c")) 3)
(check-equal? (count-group-questions '("ab" "ac")) 3)
(check-equal? (load-input-file
               "day_6_test.txt")
              '(("abc")
                ("a" "b" "c")
                ("ab" "ac")
                ("a" "a" "a" "a")
                ("b")))
(define test-groups
  (load-input-file "day_6_test.txt"))
(check-equal? (count-all-groups-questions test-groups) 11)
                  
