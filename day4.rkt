#lang racket
(require advent-of-code)

#;
(define input "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(define input (fetch-aoc-input (find-session)
                                 2022 4 #:cache #t))

(define (range-guard start end name)
  (if (<= start end)
      (values start end)
      (error "start must be <= end")))

(struct aoc-range (start end) #:transparent #:guard range-guard)

(define (between? i j k)
  (and (>= i j) (<= i k)))

(define (range-contains r1 r2)
  (and (<= (aoc-range-start r1) (aoc-range-start r2)) (<= (aoc-range-end r2) (aoc-range-end r1))))

(define (range-overlap r1 r2)
  (let ([start1 (aoc-range-start r1)]
        [start2 (aoc-range-start r2)]
        [end1 (aoc-range-end r1)]
        [end2 (aoc-range-end r2)])
    (or (between? start1 start2 end2) (between? end1 start2 end2)
        (between? start2 start1 end1) (between? end2 start1 end1))))

(define (parse-line line)
  (let ([ranges (string-split line ",")])
    (for/list ([range-s (in-list ranges)])
      (map string->number (string-split range-s "-")))))

(define (line->ranges line)
  (let ([items (parse-line line)])
    (map (λ (item)
           (aoc-range (first item) (second item)))
         items)))

(define (fully-contains? ranges)
  (or (range-contains (first ranges) (second ranges)) (range-contains (second ranges) (first ranges))))

(define (overlaps? ranges)
  (range-overlap (first ranges) (second ranges)))

(define (part1 input)
  (count identity (map (compose fully-contains? line->ranges) (string-split input "\n"))))

(define (part2 input)
  (count identity (map (compose overlaps? line->ranges) (string-split input "\n"))))

(printf "Part 1: ~v~n" (part1 input))
(printf "Part 2: ~v~n" (part2 input))