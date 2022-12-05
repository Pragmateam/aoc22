#lang racket

(require racket/trace)
(require racket/string)
(require racket/file)
(require rackunit)

(define (overlaps amin amax bmin bmax)
  (and (>= amax bmin) (<= amin bmax)))

(define (pair-contains pair)
  (let* ((a (first pair))
        (b (second pair))
        (amin (first a))
        (amax (second a))
        (bmin (first b))
        (bmax (second b)))
    (if (or (overlaps amin amax bmin bmax)
            (overlaps bmin bmax amin amax))
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

(check-equal? (solution "test/1") 4)

(solution "input")
