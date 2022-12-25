#lang racket

(require racket/file
         racket/string
         rackunit)

(define (parse input)
  (map string->list (string-split (file->string input) "\n")))


(define (snafu->number digits)
  (define mapping #hash((#\2 . 2)
                        (#\1 . 1)
                        (#\0 . 0)
                        (#\- . -1)
                        (#\= . -2)))
  (define reverse-digits (reverse digits))

  (apply +
         (for/list ([i (range (length digits))])
           (* (expt 5 i) (hash-ref mapping (list-ref reverse-digits i))))))


(define (number->snafu number)
  (define mapping #hash((2 . #\2)
                        (1 . #\1)
                        (0 . #\0)
                        (-1 . #\-)
                        (-2 . #\=)))

  (if (= number 0)
      '()
      (let ([digit (remainder number 5)])
        (if (<= digit 2)
            (append (number->snafu (quotient number 5))
                    (list (hash-ref mapping digit)))
            (append (number->snafu (+ (quotient number 5) 1))
                    (list (hash-ref mapping (- digit 5))))))))

(define (solution input)
  (list->string (number->snafu (apply + (map snafu->number (parse input))))))

(check-equal? (solution "test/1") "2=-1=0")

(solution "input")
