#lang racket

(require racket/file)
(require racket/set)
(require rackunit)

(define (find-start stream (window (list)) (offset 0))
  (if (or (= 4 (length (remove-duplicates window)))
          (empty? stream))
      offset
      (find-start
       (rest stream)
       (cons (first stream) (take window (min 3 (length window))))
       (+ 1 offset))))
(define (solution f) (find-start (string->list (file->string f))))

(check-equal? (find-start (string->list "mjqjpqmgbljsphdztnvjfqwrcgsmlb")) 7)
(check-equal? (find-start (string->list "bvwbjplbgvbhsrlpgdmjqwftvncz")) 5)
(check-equal? (find-start (string->list "nppdvjthqldpwncqszvftbrmjlhg")) 6)
(check-equal? (find-start (string->list "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")) 10)
(check-equal? (find-start (string->list "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")) 11)

(solution "input")
