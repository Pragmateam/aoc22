#lang racket

(require racket/string
         racket/file
         rackunit)

(define (signal-strength x cycle) (* x cycle))
(define (augment-input input)
  (apply append
         (for/list ([line (string-split (file->string input) "\n")])
           (match (string-split line " ")
             [`("addx" ,v) `(("noop") ("addx" ,v))]
             [`("noop") `(("noop"))]))))

(define (solution input)
  (define x 1)
  (define cycle 1)
  (define total-strength 0)

  (for ([line (augment-input input)])
    (match line [`("addx" ,v) (set! x (+ x (string->number v)))] [_ '()])
    (set! cycle (+ 1 cycle))
    (when (member cycle '(20 60 100 140 180 220))
      (set! total-strength (+ total-strength (signal-strength x cycle)))))

  total-strength)

(check-equal? (solution "test/1") 13140)

(solution "input")
