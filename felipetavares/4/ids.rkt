#lang racket

(require racket/trace)
(require racket/string)
(require racket/file)
(require rackunit)

(define (fully-contains amin amax bmin bmax)
  (and (<= amin bmin) (>= amax bmax)))

(define (pair-contains pair)
  (let* ((a (first pair))
        (b (second pair))
        (amin (first a))
        (amax (second a))
        (bmin (first b))
        (bmax (second b)))
    (if (or (fully-contains amin amax bmin bmax)
            (fully-contains bmin bmax amin amax))
        1 0)))

(define (contains pairs)
  (apply + (map pair-contains pairs)))

(define (pair->ranges pair)
  (map string->number (string-split pair "-")))

(define (line->ranges line)
  (map pair->ranges (string-split line ",")))

(define (parse f)
  (map line->ranges
       (string-split (file->string f) "\n")))

(define (solution f) (contains (parse f)))

(check-equal? (solution "test/1") 2)

(solution "input")
