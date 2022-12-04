#lang racket
(require racket/generator)
(require advent-of-code)

#;(define input "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(define input (fetch-aoc-input (find-session)
                                 2022 3 #:cache #t))

(define (line->compartments line)
  (let ([half-line-length (/ (string-length line) 2)])
    (cons (substring line 0 half-line-length) (substring line half-line-length))))

(define (common-item compartments)
  (let*-values ([(first-l) (string->list (car compartments))]
                [(second-l) (string->list (cdr compartments))])
    (set-first (set-intersect (list->set first-l) (list->set second-l)))))


(define (common-items input)
  (for/list ([line (in-list (string-split input "\n"))])
    (common-item (line->compartments line))))

(define (priority char)
  (cond 
    [(char>=? char #\a) (+ 1 (- (char->integer char) (char->integer #\a)))]
    [else (+ 27 (- (char->integer char) (char->integer #\A)))]))

(define (part1 input)
  (apply + (map priority (common-items input))))

(part1 input)

(define (three-at-a-time l) (generator ()
                                       (let process-rest ([input l])
                                         (yield (take input 3))
                                         (let ([rest (drop input 3)])
                                           (if (null? rest)
                                               '()
                                               (process-rest rest))))))


; Part 2


(define (find-badge rucksacks)
  (set-first (apply set-intersect
                    (for/list ([rucksack (in-list rucksacks)])
                      (for/set ([ch (in-string rucksack)]) ch)))))

(define grouping-gen (three-at-a-time (string-split input "\n")))

(define (part2 input)
  (apply + (map priority (for/list ([rucksack-group (in-producer grouping-gen '())])
                           (find-badge rucksack-group)))))

(part2 input)