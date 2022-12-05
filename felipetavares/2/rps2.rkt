#lang racket

(require racket/string)
(require racket/file)
(require rackunit)

(define int char->integer)
(define (sum lst) (apply + lst))
(define (shape-score shape) (+ shape 1))
(define (round-result mine theirs) (modulo (+ 1 (- mine theirs)) 3))
(define (find-move r m) (modulo (+ m r 2) 3))
(define (round-score shapes)
  (let* ((theirs (first shapes))
         (wanted-outcome (second shapes))
         (mine (find-move wanted-outcome theirs)))
    (+ (shape-score mine)
       (* 3 (round-result mine theirs)))))

(define (first-char str)
  (int (first (string->list str))))

(define (pair->shapes pair)
 `(,(- (first-char (first pair)) (int #\A))
   ,(- (first-char (second pair)) (int #\X))))

(define (strat->list strat)
  (map (lambda (line) (pair->shapes (string-split line " ")))
       (string-split (file->string strat) "\n")))

(define (match-score strat)
  (sum (map round-score (strat->list strat))))

(check-equal? (match-score "test/1") 12)

(match-score "input")
