#lang racket

(require rackunit)

(provide highest-seat-id
         load-input-file
         missing-seat)

(define seat-rows (range 128))
(define seat-columns (range 8))

(define (calculate-seat-id row column)
  (+ (* row 8) column))

(define (all-seat-ids rows columns)
  (for*/list ([row rows]
              [column columns])
    (calculate-seat-id row column)))

(define (lower-half lst)
  (take lst (mid-point lst)))

(define (upper-half lst)
  (drop lst (mid-point lst)))

(define (mid-point lst)
  (/ (length lst) 2))

(define (split-rows lst indicator)
  (split lst indicator #\F #\B))

(define (split lst indicator lower-ind upper-ind)
  (cond
    [(eq? indicator lower-ind) (lower-half lst)]
    [(eq? indicator upper-ind) (upper-half lst)]))

(define (split-columns lst indicator)
  (split lst indicator #\L #\R))

(define (find lst indicators split-func)
  (if (empty? indicators) lst (find (split-func lst (first indicators)) (rest indicators) split-func)))

(define (find-rows lst indicators)
  (find lst indicators split-rows))

(define (find-columns lst indicators)
  (find lst indicators split-columns))

(define (row-indicators boarding-pass)
  (take (string->list boarding-pass) 7))

(define (column-indicators boarding-pass)
  (drop (string->list boarding-pass) 7))

(define (seat-id boarding-pass)
  (let ([row (first (find-rows seat-rows (row-indicators boarding-pass)))]
        [column (first (find-columns seat-columns (column-indicators boarding-pass)))])
    (calculate-seat-id row column)))

(define (all-seat-ids-for-passes boarding-passes)
  (for/list ([pass boarding-passes])
    (seat-id pass)))

(define (highest-seat-id lst)
  (for/fold ([highest 0])
            ([boarding-pass lst])
    (let ([id (seat-id boarding-pass)])
      (if (> id highest) id highest))))

(define (load-input-file file-name)
  (call-with-input-file file-name
    (λ (in)
      (for/list ([l (in-lines in)])
        l))))

(define (missing-seat-ids boarding-pass-seats seat-ids)
  (set->list (set-subtract
              (list->set seat-ids)
              (list->set boarding-pass-seats))))

(define (missing-seat boarding-passes)
  (let ([boarding-pass-seats (all-seat-ids-for-passes boarding-passes)])
    (first (find-seat
            (missing-seat-ids boarding-pass-seats (all-seat-ids seat-rows seat-columns))
            boarding-pass-seats))))
   
(define (find-seat missing-seats boarding-pass-seats)
  (for/list ([id missing-seats]
             #:when (and (member (+ id 1) boarding-pass-seats) (member (- id 1) boarding-pass-seats)))
    id))

; Tests
(check-equal? (lower-half seat-rows) (range 64))
(check-equal? (upper-half seat-rows) (range 64 128))
(check-equal? (split-rows seat-rows #\F) (range 64))
(check-equal? (split-rows seat-rows #\B) (range 64 128))
(check-equal? (find-rows seat-rows (string->list "FBFBBFF")) '(44))
(check-equal? (find seat-rows (string->list "FBFBBFF") split-rows) '(44))
(check-equal? (split-columns seat-columns #\L) (range 4))
(check-equal? (find-columns seat-columns (string->list "RLR")) '(5))
(check-equal? (seat-id "BFFFBBFRRR") 567)
(check-equal? (seat-id "FFFBBBFRRR") 119)
(check-equal? (seat-id "BBFFBBFRLL") 820)
(check-equal? (highest-seat-id '("BFFFBBFRRR" "FFFBBBFRRR" "BBFFBBFRLL")) 820)
(check-equal? (calculate-seat-id 70 7) 567)
(check-equal? (all-seat-ids '(0 1 2) '(0 1)) '(0 1 8 9 16 17))
(check-equal? (missing-seat-ids '(119 820)
                                '(567 119 820))            
              '(567))
(check-equal? (find-seat '(1 20 35 89 70) '(2 99 34 100 36 22)) '(35))
