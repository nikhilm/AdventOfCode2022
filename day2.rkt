#lang racket


(module part1 racket
  
  (require advent-of-code)

  (define input (fetch-aoc-input (find-session)
                                 2022 2 #:cache #t))

  (define (char->object char)
    (case char
      [("A" "X") 'rock]
      [("B" "Y") 'paper]
      [("C" "Z") 'scissor]))

  (define (round-result objects)
    (match objects
      [(list 'rock 'paper) '(2 6)]
      [(list 'rock 'scissor) '(3 0)]
      [(list 'rock 'rock) '(1 3)]
      [(list 'paper 'rock) '(1 0)]
      [(list 'paper 'scissor) '(3 6)]
      [(list 'paper 'paper) '(2 3)]
      [(list 'scissor 'paper) '(2 0)]
      [(list 'scissor 'rock) '(1 6)]
      [(list 'scissor 'scissor) '(3 3)]))

  (define (line->round line)
    (map char->object (string-split line)))

  (define (result->score result)
    (apply + result))

  (define part1-answer
    (apply + (map (λ (x) (result->score (round-result (line->round x)))) (string-split input "\n"))))

  (provide part1-answer))

(module part2 racket
  (require advent-of-code)

  (define input (fetch-aoc-input (find-session)
                                 2022 2 #:cache #t))

  (define (char->object char)
    (case char
      [("A") 'rock]
      [("B") 'paper]
      [("C") 'scissor]
      [("X") 'need-to-lose]
      [("Y") 'need-to-draw]
      [("Z") 'need-to-win]))

  ; scissor - 3 pts
  ; paper - 2 pts
  ; rock - 1 pts
  ; win - 6 pts
  ; draw - 3 pts
  ; lose - 0 pts

  (define (round-result objects)
    (match objects
      [(list 'rock 'need-to-lose) '(3 0)] ; choose scissor
      [(list 'rock 'need-to-draw) '(1 3)] ; choose rock
      [(list 'rock 'need-to-win) '(2 6)] ; choose paper
      [(list 'paper 'need-to-lose) '(1 0)] ; choose rock
      [(list 'paper 'need-to-draw) '(2 3)] ; choose paper
      [(list 'paper 'need-to-win) '(3 6)] ; choose scissor
      [(list 'scissor 'need-to-lose) '(2 0)] ; choose paper
      [(list 'scissor 'need-to-draw) '(3 3)] ; choose scissor
      [(list 'scissor 'need-to-win) '(1 6)])) ; choose rock

  (define (line->round line)
    (map char->object (string-split line)))

  (define (result->score result)
    (apply + result))

  (define part2-answer (apply + (map (λ (x) (result->score (round-result (line->round x)))) (string-split input "\n"))))
  (provide part2-answer))


(require 'part1)
(require 'part2)
(printf "Part 1: ~v~n" part1-answer)
(printf "Part 2: ~v~n" part2-answer)