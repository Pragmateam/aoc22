#lang racket

(require racket/file)
(require racket/set)
(require rackunit)

(define (find-start stream (window (list)) (offset 0))
  (if (or (= 14 (length (remove-duplicates window)))
          (empty? stream))
      offset
      (find-start
       (rest stream)
       (cons (first stream) (take window (min 13 (length window))))
       (+ 1 offset))))
(define (solution f) (find-start (string->list (file->string f))))

(check-equal? (find-start (string->list "mjqjpqmgbljsphdztnvjfqwrcgsmlb")) 19)
(check-equal? (find-start (string->list "bvwbjplbgvbhsrlpgdmjqwftvncz")) 23)
(check-equal? (find-start (string->list "nppdvjthqldpwncqszvftbrmjlhg")) 23)
(check-equal? (find-start (string->list "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")) 29)
(check-equal? (find-start (string->list "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")) 26)

(solution "input")
