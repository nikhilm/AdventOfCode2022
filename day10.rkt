#lang br/quicklang

(require advent-of-code)

(struct crt (rows))

(define (make-row)
  (make-vector 40 #f))

(define (make-crt)
  (crt (build-vector 6 (lambda (i) (make-row)))))

(define (crt-row->string row)
  (build-string (vector-length row) (lambda (i) (if (vector-ref row i) #\# #\.))))

(define (format-crt crt)
  (define string-rows
    (for/list ([row (in-vector (crt-rows crt))])
    (crt-row->string row)))
  (string-join string-rows "\n"))
    
(define (print-crt crt)
  (printf "~a~n" (format-crt crt)))

(define (cycle->row-index cycle-num)
  (quotient (sub1 cycle-num) 40))

(define (sprite-range val)
  (list (sub1 val) val (add1 val)))

(define (cycle->col-index cycle-num)
  (remainder (sub1 cycle-num) 40))

(define (should-draw? col sprite-center)
  (member col (sprite-range sprite-center)))

(define (draw-pixel crt row col)
  (printf "r: ~a c: ~a~n" row col)
  (vector-set! (vector-ref (crt-rows crt) row) col #t))

(define X 1)
(define the-crt (make-crt))
(define cycle-num 1)
; since addx takes 2 cycles, we maintain a queue of size 2
(define queue '(0))

(define (cycle)
  (let ([front (car queue)])
    (printf "YOYO queue ~a. before cycle exec X is ~a~n" queue X)
    (let ([row (cycle->row-index cycle-num)]
          [col (cycle->col-index cycle-num)])
      (when (and (< cycle-num 240) (should-draw? col X)) (draw-pixel the-crt row col)))
    (when (memv cycle-num '(20 60 100 140 180 220))
      
        (set! signal-strength (+ signal-strength (* cycle-num X)))
        #;(printf "During cycle ~a sigsig ~a sigstr ~a~n" cycle-num (* cycle-num X) signal-strength))
    (begin
      (set! X (+ X front))
      ; pop the queue, maintaining length as required
      (set! queue (cdr queue))
      (when (empty? queue)
        (set! queue '(0)))
      (set! cycle-num (add1 cycle-num)))))

(define (noop)
  (cycle))

(define (addx n)
  ; push to the queue, then cycle
  (set! queue (append queue (list n)))
  (cycle) (cycle))

(define (make-long-enough l)
  (if (equal? (length l) 240)
      l
      (make-long-enough (cons '(noop) l))))

(define signal-strength 0)
(define (run-computer . ops)
  (let ([ops (reverse (make-long-enough (reverse ops)))])
    (printf "LENGTH ~a~n" ops)
    (for ([i (in-naturals 1)] [op (in-list ops)])
      
      (cond
        [(eq? (first op) 'addx) (addx (second op))]
        [(eq? (first op) 'noop) (noop)])
      )))

(provide noop)
(provide addx)
(provide run-computer)

(define (read-syntax path port)
  (define src-lines (filter non-empty-string? (port->lines port)))
  (define src-datums (format-datums '(~a) src-lines))
  (datum->syntax #f `(module day10-r "day10.rkt"
                       ,@src-datums)))
(provide read-syntax)

(define-macro (day10-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     (run-computer 'HANDLE-EXPR ...)
     (printf "signal strength is ~a~n" signal-strength)
     (print-crt the-crt)))
(provide (rename-out [day10-module-begin #%module-begin]))
