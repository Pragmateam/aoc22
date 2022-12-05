#lang racket

(require racket/string)
(require racket/file)
(require racket/set)
(require rackunit)

(define (priority type)
  (let ((ascii (char->integer type))
        (a (char->integer #\a))
        (A (char->integer #\A)))
    (if (>= ascii a)
        (+ (- ascii a) 1)
        (+ (- ascii A) 27))))

(define (parse f) (string-split (file->string f) "\n"))

(define (badge-sum items)
  (if (empty? items)
      0
      (+ (badge-sum (drop items 3))
         (priority (set-first (apply set-intersect (map (compose list->set string->list) (take items 3))))))))

(define (reorganize file-name)
  (badge-sum (parse file-name)))

(module+ test
  (define (example)
    (check-equal? (reorganize "tests/1") 70)))

(reorganize "input")
