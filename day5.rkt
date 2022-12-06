#lang racket

; Instead of dealing with any complicated parsing for the initial stacks,
; just hand-write them since they are so small.

(struct my-stack (contents) #:transparent #:mutable)

(define/contract (stack-push stack v)
  (my-stack? any/c . -> . any/c)
  (set-my-stack-contents! stack (cons v (my-stack-contents stack))))

(define/contract (stack-pop stack)
  (my-stack? . -> . any/c)
  (let ([top (car (my-stack-contents stack))])
    (set-my-stack-contents! stack (cdr (my-stack-contents stack)))
    top))

(define/contract (stack-pop-n stack n)
  (my-stack? number? . -> . any/c)
  (for/list ([i n])
    (stack-pop stack)))

(define/contract (stack-top stack)
  (my-stack? . -> . any/c)
  (car (my-stack-contents stack)))

(define (stacks-message stacks)
  (list->string (for/list ([stack (in-vector stacks)])
                  (stack-top stack))))

; This is the test input
#;
(define the-input-stacks (vector
                          (my-stack '(#\N #\Z))
                          (my-stack '(#\D #\C #\M))
                          (my-stack '(#\P))
                          ))

#|
    [G]         [P]         [M]    
    [V]     [M] [W] [S]     [Q]    
    [N]     [N] [G] [H]     [T] [F]
    [J]     [W] [V] [Q] [W] [F] [P]
[C] [H]     [T] [T] [G] [B] [Z] [B]
[S] [W] [S] [L] [F] [B] [P] [C] [H]
[G] [M] [Q] [S] [Z] [T] [J] [D] [S]
[B] [T] [M] [B] [J] [C] [T] [G] [N]
 1   2   3   4   5   6   7   8   9 
|#

(define the-input-stacks (vector
                          (my-stack (string->list "CSGB"))
                          (my-stack (string->list "GVNJHWMT"))
                          (my-stack (string->list "SQM"))
                          (my-stack (string->list "MNWTLSB"))
                          (my-stack (string->list "PWGVTFZJ"))
                          (my-stack (string->list "SHQGBTC"))
                          (my-stack (string->list "WBPJT"))
                          (my-stack (string->list "MQTFZCDG"))
                          (my-stack (string->list "FPBHSN"))))

#;
(define input "move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(define input "move 2 from 4 to 2
move 6 from 9 to 7
move 4 from 7 to 2
move 2 from 4 to 1
move 2 from 6 to 7
move 1 from 3 to 8
move 4 from 7 to 1
move 2 from 3 to 2
move 3 from 8 to 5
move 3 from 1 to 4
move 12 from 2 to 5
move 2 from 6 to 8
move 12 from 5 to 8
move 3 from 7 to 9
move 18 from 8 to 9
move 2 from 8 to 6
move 3 from 2 to 3
move 14 from 9 to 4
move 1 from 1 to 3
move 7 from 9 to 3
move 1 from 2 to 1
move 8 from 4 to 5
move 5 from 6 to 3
move 2 from 7 to 9
move 3 from 4 to 9
move 4 from 9 to 6
move 4 from 6 to 1
move 8 from 4 to 6
move 10 from 1 to 2
move 13 from 3 to 2
move 17 from 5 to 9
move 2 from 5 to 1
move 9 from 9 to 7
move 1 from 3 to 6
move 2 from 1 to 8
move 11 from 2 to 4
move 5 from 6 to 8
move 1 from 6 to 3
move 1 from 1 to 4
move 3 from 8 to 6
move 3 from 2 to 8
move 9 from 7 to 9
move 4 from 4 to 7
move 1 from 9 to 5
move 15 from 9 to 7
move 7 from 8 to 3
move 1 from 5 to 6
move 2 from 6 to 9
move 8 from 2 to 6
move 3 from 4 to 3
move 1 from 2 to 5
move 4 from 9 to 3
move 1 from 3 to 4
move 13 from 6 to 2
move 1 from 5 to 1
move 4 from 4 to 9
move 6 from 3 to 2
move 11 from 2 to 7
move 6 from 3 to 4
move 3 from 3 to 2
move 1 from 3 to 4
move 1 from 1 to 3
move 3 from 9 to 2
move 1 from 3 to 1
move 4 from 7 to 1
move 1 from 9 to 5
move 5 from 1 to 4
move 11 from 2 to 4
move 1 from 5 to 3
move 1 from 2 to 3
move 12 from 4 to 2
move 2 from 7 to 2
move 7 from 4 to 3
move 5 from 4 to 1
move 7 from 7 to 6
move 4 from 1 to 8
move 1 from 8 to 5
move 8 from 3 to 2
move 4 from 7 to 4
move 13 from 7 to 1
move 2 from 8 to 6
move 5 from 4 to 9
move 1 from 3 to 6
move 1 from 5 to 8
move 1 from 2 to 9
move 4 from 2 to 6
move 2 from 8 to 6
move 10 from 1 to 3
move 4 from 9 to 4
move 2 from 1 to 3
move 5 from 2 to 9
move 4 from 9 to 2
move 1 from 1 to 2
move 13 from 2 to 4
move 15 from 4 to 5
move 3 from 6 to 8
move 8 from 3 to 8
move 1 from 4 to 2
move 14 from 5 to 1
move 1 from 5 to 4
move 1 from 4 to 2
move 8 from 6 to 7
move 3 from 6 to 2
move 2 from 9 to 1
move 8 from 8 to 7
move 9 from 1 to 5
move 7 from 5 to 3
move 14 from 7 to 9
move 2 from 2 to 3
move 7 from 2 to 1
move 1 from 6 to 1
move 4 from 9 to 2
move 8 from 3 to 6
move 2 from 4 to 3
move 4 from 3 to 5
move 5 from 5 to 7
move 2 from 6 to 9
move 6 from 6 to 2
move 4 from 2 to 3
move 1 from 6 to 2
move 2 from 7 to 8
move 13 from 9 to 5
move 2 from 7 to 1
move 14 from 1 to 5
move 15 from 5 to 7
move 3 from 8 to 7
move 5 from 3 to 5
move 6 from 5 to 7
move 4 from 1 to 7
move 1 from 2 to 5
move 3 from 2 to 8
move 11 from 5 to 2
move 10 from 7 to 1
move 1 from 3 to 4
move 10 from 2 to 9
move 1 from 5 to 8
move 6 from 7 to 3
move 1 from 4 to 6
move 2 from 3 to 8
move 1 from 2 to 1
move 4 from 3 to 9
move 3 from 1 to 6
move 2 from 7 to 1
move 1 from 5 to 6
move 1 from 3 to 8
move 4 from 1 to 4
move 5 from 2 to 9
move 3 from 1 to 4
move 18 from 9 to 7
move 4 from 8 to 4
move 3 from 1 to 2
move 1 from 9 to 7
move 1 from 4 to 7
move 1 from 6 to 2
move 1 from 2 to 5
move 25 from 7 to 3
move 7 from 4 to 2
move 8 from 7 to 9
move 4 from 8 to 6
move 1 from 8 to 5
move 4 from 6 to 5
move 2 from 9 to 5
move 3 from 5 to 8
move 4 from 6 to 4
move 12 from 3 to 5
move 11 from 3 to 2
move 13 from 5 to 8
move 4 from 9 to 6
move 7 from 4 to 9
move 2 from 6 to 2
move 12 from 2 to 7
move 1 from 6 to 3
move 1 from 5 to 6
move 2 from 5 to 3
move 15 from 8 to 6
move 4 from 6 to 7
move 1 from 5 to 1
move 10 from 2 to 8
move 8 from 8 to 3
move 8 from 6 to 8
move 2 from 7 to 6
move 9 from 9 to 7
move 8 from 8 to 9
move 1 from 1 to 3
move 1 from 2 to 7
move 7 from 3 to 1
move 3 from 8 to 5
move 3 from 1 to 6
move 7 from 9 to 2
move 2 from 3 to 7
move 5 from 7 to 9
move 17 from 7 to 5
move 2 from 7 to 6
move 10 from 6 to 3
move 1 from 1 to 3
move 6 from 9 to 3
move 1 from 2 to 9
move 2 from 7 to 9
move 2 from 9 to 7
move 1 from 5 to 8
move 1 from 8 to 5
move 6 from 2 to 5
move 1 from 6 to 1
move 5 from 3 to 5
move 1 from 6 to 8
move 1 from 7 to 9
move 2 from 9 to 3
move 15 from 5 to 2
move 2 from 1 to 8
move 2 from 3 to 7
move 2 from 8 to 3
move 3 from 5 to 9
move 1 from 8 to 6
move 1 from 9 to 6
move 3 from 7 to 6
move 17 from 3 to 4
move 1 from 1 to 2
move 6 from 2 to 9
move 16 from 4 to 1
move 4 from 6 to 8
move 9 from 5 to 6
move 8 from 6 to 2
move 2 from 9 to 5
move 2 from 3 to 5
move 1 from 6 to 2
move 1 from 4 to 8
move 14 from 1 to 3
move 8 from 5 to 3
move 20 from 3 to 1
move 1 from 8 to 2
move 1 from 9 to 6
move 1 from 6 to 7
move 1 from 7 to 3
move 22 from 1 to 2
move 3 from 3 to 6
move 27 from 2 to 8
move 2 from 2 to 8
move 2 from 6 to 9
move 2 from 9 to 4
move 2 from 4 to 8
move 1 from 1 to 3
move 14 from 8 to 5
move 1 from 3 to 9
move 3 from 9 to 2
move 5 from 2 to 8
move 10 from 2 to 9
move 1 from 6 to 7
move 1 from 7 to 5
move 7 from 5 to 2
move 2 from 9 to 2
move 1 from 6 to 2
move 2 from 9 to 5
move 3 from 5 to 6
move 6 from 5 to 3
move 1 from 5 to 6
move 4 from 3 to 9
move 2 from 9 to 8
move 3 from 9 to 5
move 23 from 8 to 1
move 2 from 6 to 1
move 1 from 5 to 7
move 2 from 3 to 5
move 2 from 9 to 5
move 4 from 9 to 7
move 2 from 9 to 4
move 1 from 5 to 4
move 5 from 8 to 5
move 2 from 6 to 2
move 3 from 7 to 3
move 1 from 3 to 4
move 3 from 2 to 8
move 4 from 1 to 6
move 2 from 6 to 3
move 4 from 1 to 2
move 3 from 8 to 1
move 13 from 2 to 5
move 4 from 3 to 2
move 14 from 5 to 7
move 5 from 2 to 7
move 18 from 7 to 9
move 4 from 4 to 7
move 2 from 5 to 4
move 17 from 9 to 5
move 1 from 9 to 1
move 1 from 7 to 2
move 5 from 7 to 2
move 18 from 1 to 4
move 1 from 7 to 3
move 1 from 3 to 6
move 2 from 1 to 3
move 1 from 6 to 5
move 2 from 6 to 8
move 1 from 8 to 9
move 1 from 8 to 3
move 13 from 4 to 5
move 1 from 1 to 6
move 3 from 2 to 4
move 1 from 6 to 1
move 3 from 2 to 9
move 3 from 3 to 1
move 5 from 4 to 5
move 30 from 5 to 3
move 1 from 4 to 6
move 1 from 9 to 8
move 1 from 9 to 6
move 21 from 3 to 7
move 3 from 1 to 6
move 1 from 1 to 4
move 1 from 9 to 6
move 1 from 8 to 2
move 1 from 3 to 6
move 1 from 9 to 3
move 5 from 4 to 8
move 1 from 2 to 4
move 9 from 5 to 7
move 2 from 5 to 9
move 2 from 8 to 2
move 2 from 6 to 3
move 1 from 4 to 1
move 4 from 3 to 8
move 2 from 9 to 2
move 4 from 2 to 6
move 1 from 1 to 4
move 2 from 6 to 9
move 2 from 5 to 4
move 1 from 3 to 1
move 1 from 1 to 3
move 2 from 9 to 1
move 5 from 3 to 5
move 1 from 1 to 8
move 4 from 6 to 4
move 5 from 5 to 6
move 18 from 7 to 5
move 1 from 3 to 4
move 12 from 7 to 5
move 15 from 5 to 6
move 1 from 5 to 8
move 1 from 3 to 7
move 1 from 1 to 2
move 1 from 2 to 4
move 1 from 7 to 9
move 2 from 8 to 2
move 1 from 2 to 4
move 4 from 4 to 2
move 1 from 2 to 1
move 1 from 9 to 8
move 4 from 6 to 4
move 3 from 2 to 6
move 1 from 2 to 6
move 8 from 4 to 3
move 1 from 1 to 3
move 6 from 6 to 1
move 1 from 3 to 6
move 5 from 1 to 7
move 10 from 5 to 9
move 3 from 9 to 8
move 7 from 6 to 2
move 1 from 7 to 8
move 3 from 5 to 8
move 3 from 6 to 2
move 6 from 8 to 9
move 1 from 5 to 3
move 2 from 3 to 1
move 2 from 4 to 8
move 6 from 6 to 9
move 1 from 1 to 4
move 17 from 9 to 2
move 1 from 4 to 1
move 2 from 7 to 8
move 1 from 9 to 8
move 3 from 8 to 4
move 3 from 1 to 4
move 9 from 8 to 2
move 1 from 8 to 4
move 12 from 2 to 7
move 4 from 7 to 4
move 1 from 8 to 1
move 10 from 4 to 2
move 3 from 3 to 2
move 1 from 9 to 7
move 11 from 7 to 3
move 1 from 3 to 1
move 2 from 3 to 9
move 1 from 3 to 7
move 2 from 1 to 9
move 1 from 6 to 5
move 7 from 3 to 6
move 1 from 7 to 3
move 3 from 3 to 4
move 1 from 5 to 7
move 2 from 4 to 3
move 2 from 4 to 8
move 1 from 7 to 6
move 2 from 6 to 8
move 1 from 9 to 2
move 1 from 9 to 5
move 1 from 5 to 1
move 1 from 8 to 6
move 1 from 3 to 2
move 4 from 6 to 1
move 5 from 1 to 4
move 11 from 2 to 4
move 2 from 8 to 2
move 1 from 8 to 9
move 27 from 2 to 5
move 4 from 6 to 3
move 3 from 2 to 4
move 2 from 5 to 9
move 1 from 5 to 7
move 2 from 9 to 5
move 14 from 4 to 7
move 2 from 4 to 7
move 3 from 4 to 8
move 4 from 3 to 1
move 4 from 1 to 8
move 2 from 3 to 9
move 2 from 9 to 3
move 7 from 8 to 9
move 1 from 3 to 8
move 2 from 3 to 2
move 25 from 5 to 9
move 1 from 5 to 8
move 1 from 8 to 7
move 26 from 9 to 1
move 23 from 1 to 5
move 7 from 9 to 7
move 1 from 9 to 8
move 1 from 9 to 2
move 5 from 7 to 1
move 20 from 5 to 6
move 1 from 7 to 6
move 2 from 5 to 3
move 1 from 8 to 6
move 21 from 6 to 8
move 1 from 6 to 4
move 1 from 1 to 7
move 2 from 1 to 6
move 1 from 1 to 3
move 1 from 2 to 5
move 1 from 2 to 6
move 2 from 7 to 6
move 6 from 7 to 9
move 3 from 1 to 2
move 17 from 8 to 1
move 1 from 4 to 1
move 2 from 6 to 9
move 3 from 8 to 9
move 2 from 3 to 7
move 2 from 9 to 8
move 4 from 7 to 3
move 4 from 3 to 4
move 2 from 5 to 8
move 4 from 8 to 4
move 3 from 6 to 8
move 18 from 1 to 5
move 1 from 3 to 4
move 3 from 2 to 4
move 5 from 9 to 1
move 10 from 7 to 5
move 5 from 1 to 3
move 5 from 3 to 5
move 5 from 4 to 3
move 2 from 4 to 2
move 5 from 8 to 3
move 25 from 5 to 2
move 3 from 3 to 6
move 1 from 1 to 3
move 3 from 6 to 7
move 1 from 4 to 2
move 1 from 5 to 8
move 2 from 4 to 9
move 1 from 8 to 1
move 20 from 2 to 7
move 10 from 7 to 1
move 1 from 1 to 7
move 4 from 7 to 8
move 5 from 5 to 4
move 4 from 8 to 6
move 1 from 1 to 3
move 5 from 7 to 4
move 2 from 1 to 5
move 4 from 9 to 1
move 3 from 2 to 5
move 5 from 5 to 1
move 1 from 9 to 1
move 11 from 1 to 3
move 1 from 6 to 2
move 7 from 3 to 5
move 11 from 3 to 7
move 1 from 2 to 6
move 7 from 7 to 8
move 1 from 9 to 1
move 2 from 3 to 1
move 1 from 5 to 3
move 4 from 1 to 6
move 4 from 6 to 3
move 9 from 4 to 5
move 2 from 8 to 2
move 4 from 6 to 9
move 3 from 2 to 4
move 1 from 8 to 6")

(define move-pat #px"move (\\d+) from (\\d+) to (\\d+)")

(define (line->stack-op line)
  ; a stack op is '(number   from-stack-idx (0-based)  to-stack-idx)
  (let ([converted (map string->number (cdr (regexp-match move-pat line)))])
    (match converted
      [(list n from to) (list n (sub1 from) (sub1 to))])))

(define (lines->stack-ops lines)
  (for/list ([line (in-list (string-split lines "\n"))])
    (line->stack-op line)))

; modifies stacks with one operation
(define (move-crates stacks op)
  (match op
    [(list n from to)
     (for ([i n])
       (let ([from-stack (vector-ref stacks from)]
             [to-stack (vector-ref stacks to)])
         (stack-push to-stack (stack-pop from-stack))))]))

(define (move-all-crates stacks ops)
  (for ([op (in-list ops)])
    (move-crates stacks op)))

(define (part1 stacks input)
  (move-all-crates stacks (lines->stack-ops input))
  (stacks-message stacks))

(define (move-crates-together stacks op)
  (match op
    [(list n from to)
     (let* ([from-stack (vector-ref stacks from)]
            [to-stack (vector-ref stacks to)]
            [popped (stack-pop-n from-stack n)])
       (for ([item (reverse popped)])
         (stack-push to-stack item)))]))

(define (move-all-crates-together stacks ops)
  (for ([op (in-list ops)])
    (move-crates-together stacks op)))

(define (part2 stacks input)
  (move-all-crates-together stacks (lines->stack-ops input))
  (stacks-message stacks))

(define (copy-stacks stacks)
  (vector-map (lambda (stack) (struct-copy my-stack stack)) stacks))

(printf "Part 1: ~v~n" (part1 (copy-stacks the-input-stacks) input))
(printf "Part 2: ~v~n" (part2 (copy-stacks the-input-stacks) input))