#lang racket

(require racket/file
         racket/string
         rackunit)

(define (parse input)
  (define (parse-operation op)
    (match (string-split op " ")
      [`(,a ,op ,b)
       `(,(string->symbol op) ,a ,b)]
      [`(,n) (string->number n)]))

  (define (parse-line line)
    (match-let ([`(,name ,operation) (string-split line ":")])
      (cons name (parse-operation (string-trim operation)))))

  (make-hash (map parse-line (string-split (file->string input) "\n"))))

(define (monkey-eval monkeys m)
  (match (hash-ref monkeys m)
    ; Just yell out a number
    [(? number? n) n]
    ; Do an operation
    [`(,op ,a ,b) (eval `(,op ,(monkey-eval monkeys a) ,(monkey-eval monkeys b)))]))

(define (solution input)
  (monkey-eval (parse input) "root"))

(check-equal? (solution "test/1") 152)

(solution "input")
