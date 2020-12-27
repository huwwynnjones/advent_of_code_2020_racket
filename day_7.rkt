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
    (if (equal? number "no")
        '()
        (list (string->number number) (string-join (list hue colour))))))

(define (third-bag text)
  (let* ([third-bag-section (string-split (second (string-split text ",")))]
         [number (first third-bag-section)]
         [hue (second third-bag-section)]
         [colour (third third-bag-section)])
    (list (string->number number) (string-join (list hue colour)))))

(define (third-bag-section? text)
  (string-contains? text ","))
    
(define (load-input-file filename)
  (call-with-input-file filename
    (λ (in)
      (create-bag-map (in-lines in)))))

(define (create-bag-rule-pairs rules)
  (for/list ([rule rules])
    (split-text rule)))

(define (create-bag-map lines)
  (make-hash (create-bag-rule-pairs lines)))


;Tests
(define test-rule "light red bags contain 1 bright white bag, 2 muted yellow bags.")
(check-equal? (split-text test-rule)
              (list "light red" (list (list 1 "bright white")
                                      (list 2 "muted yellow"))))
(check-equal? (split-text "faded blue bags contain no other bags.")
              (list "faded blue" (list '())))
(check-equal? (first-bag test-rule)
              "light red")
(check-equal? (second-bag test-rule)
              '(1 "bright white"))
(check-equal? (third-bag test-rule)
              '(2 "muted yellow"))
(check-equal? (third-bag-section? test-rule) #t)
(check-equal? (third-bag-section? "bright white bags contain 1 shiny gold bag.") #f)
(define test-bags
  (make-hash (list
              (list "light red" (list (list 1 "bright white") (list 2 "muted yellow")))
              (list "dark orange" (list (list 3 "bright white") (list 4 "muted yellow")))
              (list "bright white" (list (list 1 "shiny gold")))
              (list "muted yellow" (list (list 2 "shiny gold") (list 9 "faded blue")))
              (list "shiny gold" (list (list 1 "dark olive") (list 2 "vibrant plum")))
              (list "dark olive" (list (list 3 "faded blue") (list 4 "dotted black")))
              (list "vibrant plum" (list (list 5 "faded blue") (list 6 "dotted black")))
              (list "faded blue" '(()))
              (list "dotted black" '(())))))
(check-equal? (load-input-file "day_7_test.txt")
              test-bags)
