#lang racket/base

(require racket/string)
(require racket/file)
(require racket/list)
(require rackunit)

(define (sum lst) (apply + lst))
(define (split-numbers str) (string-split str "\n"))
(define (split-elves str) (string-split str "\n\n"))
(define (strs->nums str-list) (map string->number str-list))
(define (top3-calories calories) (sum (take (sort (map sum calories) >) 3)))

(define (string->elves-calories str)
  (map strs->nums (map split-numbers (split-elves str))))

(define (string->top3-calories str)
  (top3-calories (string->elves-calories str)))

(define (file->top3-calories file)
  (string->top3-calories (file->string file)))

(check-equal? (file->top3-calories "test/1") 45000)

(file->top3-calories "input")
