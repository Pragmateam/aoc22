#lang racket

(require racket/file
         racket/string
         rackunit)

(define (parse input)
  (define (parse-operation name op)
    (match (string-split op " ")
      [`(,a ,op ,b)
       `(,(cond
            [(equal? name "root") '=]
            [else (string->symbol op)])
         ,a
         ,b)]
      [`(,n)
       (cond
         [(equal? name "humn") 'x]
         [else (string->number n)])]))

  (define (parse-line line)
    (match-let ([`(,name ,operation) (string-split line ":")])
      (cons name (parse-operation name (string-trim operation)))))

  (make-hash (map parse-line (string-split (file->string input) "\n"))))

(define (monkey-eval monkeys m)
  (match (hash-ref monkeys m)
    ; Just yell out a number
    [(? number? n) n]
    ; Do an operation
    [`(,op ,a ,b) (eval `(,op ,(monkey-eval monkeys a) ,(monkey-eval monkeys b)))]))

(define (monkey-expression monkeys m)
  (match (hash-ref monkeys m)
    ['x 'x]
    [(? number? n) n]
    [`(,op ,a ,b) `(,op ,(monkey-expression monkeys a)
                        ,(monkey-expression monkeys b))]))

(define (split-expression expr)
  (match expr
    [`(,op ,a ,b) (values op a b)]))

(define (contains-x? expr)
  (match expr
    [(? number? _) #f]
    ['x #t]
    [`(,_ ,a ,b) (or (contains-x? a) (contains-x? b))]))

(define (invert-operation op)
  (match op ['+ '-] ['- '+] ['* '/] ['/ '*]))

(define (closed-form-solution expr)
  (match expr
    [`(= x ,expr) expr]
    [`(= ,a ,b)
     (let-values ([(op in1 in2) (split-expression a)])
       (if (contains-x? in1)
           (closed-form-solution
            `(= ,in1 (,(invert-operation op) ,b ,in2)))
           (cond
             ; Those are not commutative
             [(equal? op '-)
              (closed-form-solution
               `(= ,in2 (,op ,in1 ,b)))]
             [(equal? op '/)
              (closed-form-solution
               `(= ,in2 (,op ,in1 ,b)))]
             [else
              (closed-form-solution
               `(= ,in2 (,(invert-operation op) ,b ,in1)))])))]))

(define (solution input)
  (eval (closed-form-solution (monkey-expression (parse input) "root"))))

(check-equal? (solution "test/1") 301)

(solution "input")
