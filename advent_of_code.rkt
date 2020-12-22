#lang racket

(require "day_1.rkt"
         (rename-in "day_5.rkt" [load-input-file day-5-load-input-file])
         (rename-in "day_6.rkt" [load-input-file day-6-load-input-file]))

(displayln "Day 1 part 1")
(define input (load-input-file "day_1.txt"))
(define answer (find-two-that-sum-2020 input))
(displayln (format "Answer is ~a" (* (first answer) (second answer))))

(displayln "Day 5 part 1")
(define day-5-input (day-5-load-input-file "day_5.txt"))
(define day-5-answer (highest-seat-id day-5-input))
(displayln (format "Answer is ~a" day-5-answer))
(displayln "Day 5 part 2")
(displayln (format "Answer is ~a" (missing-seat day-5-input)))

(displayln "Day 6 part 1")
(define day-6-input (day-6-load-input-file "day_6.txt"))
(displayln (format "Answer is ~a" (count-all-groups-questions day-6-input 'no-dups)))
(displayln "Day 6 part 2")
(displayln (format "Answer is ~a" (count-all-groups-questions day-6-input 'intersect)))
