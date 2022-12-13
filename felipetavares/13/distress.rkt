#lang racket

(require racket/file
         racket/string
         rackunit)

(define (parse input)
  (read (open-input-string (string-join `("(" ,(string-replace (file->string input) "," " ") ")")))))

(define (compare-pair pair)
  (match pair
    [`(,l ,r) #:when (and (integer? l) (integer? r))
              (cond
                [(< l r) 'good]
                [(> l r) 'bad]
                [(= l r) 'continue])]
    [`(,l ,r) #:when (and (list? l) (list? r))
              (cond
                [(and (empty? l) (empty? r)) 'continue]
                [(empty? l) 'good]
                [(empty? r) 'bad]
                [else
                 (match (compare-pair (list (first l) (first r)))
                   ['continue (compare-pair (list (rest l) (rest r)))]
                   [sym sym])])]
    [`(,l ,r) #:when (and (integer? l) (list? r))
              (compare-pair (list `(,l) r))]
    [`(,l ,r) #:when (and (list? l) (integer? r))
              (compare-pair (list l `(,r)))]))

(define (compare-pairs pairs)
  (if (empty? pairs)
      '()
      (cons (compare-pair (take pairs 2))
            (compare-pairs (drop pairs 2)))))

(define (solution input)
  (define sum 0)
  (define cmp (compare-pairs (parse input)))

  (for ([i (inclusive-range 1 (length cmp))])
    (when (member (list-ref cmp (sub1 i)) '(good continue))
      (set! sum (+ sum i))))

  sum)

(check-equal? (solution "test/1") 13)

(solution "input")
