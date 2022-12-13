#lang racket

(require racket/file
         racket/string
         rackunit)

(define (parse input)
  (read (open-input-string (string-join `("(" ,(string-replace (file->string input) "," " ") ")")))))

(define (compare-pair l r)
  (cond
    [(and (integer? l) (integer? r))
     (cond
       [(< l r) #t]
       [(> l r) #f]
       [(= l r) 'continue])]
    [(and (list? l) (list? r))
     (cond
       [(and (empty? l) (empty? r)) 'continue]
       [(empty? l) #t]
       [(empty? r) #f]
       [else
        (match (compare-pair (first l) (first r))
          ['continue (compare-pair (rest l) (rest r))]
          [sym sym])])]
    [(and (integer? l) (list? r)) (compare-pair `(,l) r)]
    [(and (list? l) (integer? r)) (compare-pair l `(,r))]))

(define (solution input)
  (define sorted (sort (append '(((2)) ((6))) (parse input)) compare-pair))
  (* (add1 (index-of sorted '((2))))
     (add1 (index-of sorted '((6))))))

(check-equal? (solution "test/1") 140)

(solution "input")
