#lang racket

(require racket/string
         racket/file
         rackunit)

(define (augment-input input)
  (apply append
         (for/list ([line (string-split (file->string input) "\n")])
           (match (string-split line " ")
             [`("addx" ,v) `(("noop") ("addx" ,v))]
             [`("noop") `(("noop"))]))))

(define (solution input)
  (define x 1)
  (define crt "")
  (define lines (augment-input input))

  (for ([line lines]
        [cycle (range (length lines))])
    (define pos (+ 1 (remainder cycle 40)))
    (if (and (>= pos x) (<= pos (+ x 2)))
        (set! crt (string-append crt "#"))
        (set! crt (string-append crt ".")))

    (when (eq? (first line) "addx")
      (set! x (+ x (string->number (second line)))))

    (when (= pos 40)
      (set! crt (string-append crt "\n"))))

  crt)

(check-equal? (solution "test/1") "##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....
")

(display (solution "input"))
