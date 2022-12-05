#lang racket/base

(require racket/string)
(require racket/file)
(require rackunit)

(define (sum lst) (apply + lst))
(define (split-numbers str) (string-split str "\n"))
(define (split-elves str) (string-split str "\n\n"))
(define (strs->nums str-list) (map string->number str-list))
(define (max-calories calories) (apply max (map sum calories)))

(define (string->elves-calories str)
  (map strs->nums (map split-numbers (split-elves str))))

(define (string->max-calories str)
  (max-calories (string->elves-calories str)))

(define (file->max-calories file)
  (string->max-calories (file->string file)))

(check-equal? (file->max-calories "test/1") 24000)

(file->max-calories "input")
