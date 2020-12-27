#lang racket

(require rackunit)

(define (split-text text)
  (if (third-bag-section? text)
      (list (first-bag text)
            (list (second-bag text)
                  (third-bag text)))
      (list (first-bag text)
            (list (second-bag text)))))

(define (first-bag text)
  (let* ([words (string-split text)]
         [hue (first words)]
         [colour (second words)])
    (string-join (list hue colour))))

(define (second-bag text)
  (let* ([words (string-split text)]
         [number (fifth words)]
         [hue (sixth words)]
         [colour (seventh words)])
    (list (string->number number) (string-join (list hue colour)))))

(define (third-bag text)
  (let* ([third-bag-section (string-split (second (string-split text ",")))]
         [number (first third-bag-section)]
         [hue (second third-bag-section)]
         [colour (third third-bag-section)])
    (list (string->number number) (string-join (list hue colour)))))

(define (third-bag-section? text)
  (string-contains? text ","))
    


;Tests
(define test-rule "light red bags contain 1 bright white bag, 2 muted yellow bags.")
(check-equal? (split-text test-rule)
              (list "light red" (list (list 1 "bright white")
                                  (list 2 "muted yellow"))))
