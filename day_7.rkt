#lang racket

(require rackunit)

(provide count-bags
         count-all-bags
         load-input-file)

(define (split-text text)
  (if (third-bag-section? text)
      (list (first-bag text)
            (append
             (list (second-bag text))
             (other-bags text)))
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

(define (other-bags text)
  (let ([other-bags-section (cdr (string-split text ","))])
    (for/list ([bags other-bags-section])
      (let* ([bag (string-split bags)]
             [number (first bag)]
             [hue (second bag)]
             [colour (third bag)])
        (list (string->number number) (string-join (list hue colour)))))))

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

(define (count-bags bags-map colour)
  (count (λ (c) (not (equal? c colour)))
         (remove-duplicates
          (flatten
           (find-all-bags bags-map colour)))))

(define (find-all-bags bags-map colour)
  (list colour
        (for/list ([bag (find-bags-held bags-map colour)])
          (find-all-bags bags-map bag))))

(define (find-bags-held bags-map colour)
  (flatten (hash-map bags-map (lambda (k v)
                                (if (member colour (flatten v)) k null)))))

(define (count-all-bags bags-map colour)
  (let ([bags (list-of-bags-inside bags-map colour)])
    (for/sum ([bag bags])
      (if (empty? bag)
          0
          (let ([bag-count (car bag)]
                [child-bag-count
                 (count-all-bags bags-map (cadr bag))])
            (if (= child-bag-count 0)
                bag-count
                (+ bag-count (* bag-count child-bag-count))))))))

(define (list-of-bags-inside bags-map colour)
  (car (hash-ref bags-map colour)))
  

;Tests
(define test-rule "light red bags contain 1 bright white bag, 2 muted yellow bags.")
(define four-bag-rule "dark tomato bags contain 1 posh teal bag, 1 posh lavender bag, 5 dim cyan bags, 4 light yellow bags.")
(check-equal? (split-text test-rule)
              (list "light red" (list (list 1 "bright white")
                                      (list 2 "muted yellow"))))
(check-equal? (split-text four-bag-rule)
              (list "dark tomato" (list (list 1 "posh teal")
                                        (list 1 "posh lavender")
                                        (list 5 "dim cyan")
                                        (list 4 "light yellow"))))
(check-equal? (split-text "faded blue bags contain no other bags.")
              (list "faded blue" (list '())))
(check-equal? (first-bag test-rule)
              "light red")
(check-equal? (second-bag test-rule)
              '(1 "bright white"))
(check-equal? (other-bags
               four-bag-rule)
              '((1 "posh lavender")
                (5 "dim cyan")
                (4 "light yellow")))
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
(check-equal? (count-bags test-bags "shiny gold") 4)
(check-equal? (count-all-bags test-bags "shiny gold") 32)
