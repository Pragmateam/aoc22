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

(define (parse-compartments items)
  (let ((middle (/ (string-length items) 2)))
    (map (compose list->set string->list)
         (list
          (substring items 0 middle)
          (substring items middle)))))

(define (parse f)
  (map parse-compartments (string-split (file->string f) "\n")))

(define (reorganize file-name)
  (apply +
         (map (lambda (sack)
                (priority (set-first (set-intersect (first sack) (second sack)))))
              (parse file-name))))

(module+ test
  (define (example)
    (check-equal? (reorganize "tests/1") 157)))

(reorganize "input")
