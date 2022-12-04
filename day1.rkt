#lang racket
(require advent-of-code)
#;
(define input "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
")

(define input (fetch-aoc-input (find-session)
                   2022 1 #:cache #t))

(define input-list (string-split input "\n" #:trim? #f))
(define as-ints (for/list ([item (in-list input-list)])
                  (cond
                    [(non-empty-string? item) (string->number item)]
                    [else 'yo])))


(define (split-first l)
  (splitf-at l (λ (x) (not (eq? x 'yo)))))


(define (to-sublists l)
  (cond
    [(empty? l) '()]
    [else (let-values ([(first remaining) (split-first l)])
     (cons first (to-sublists (cdr remaining))))]))

(define sublists (to-sublists as-ints))
(define sums
  (for/list ([l (in-list sublists)])
    (apply + l)))

(printf "Part 1: ~v~n" (apply max sums))

(define max1 (apply max sums))
(define remaining (remq max1 sums))
(define max2 (apply max remaining))
(define remaining2 (remq max2 remaining))
(define max3 (apply max remaining2))

(printf "Part 2: ~v~n" (+ max1 max2 max3))
