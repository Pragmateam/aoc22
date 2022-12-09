#lang racket

(require racket/string)
(require racket/file)
(require algorithms)
(require rackunit)

(struct vec (x y))

(define (vec-add a b) (vec (+ (vec-x a) (vec-x b)) (+ (vec-y a) (vec-y b))))
(define (sign n) (match (positive? n) [#t 1] [#f -1]))

(define (normalize v)
  (let ([longest (max (abs (vec-x v)) (abs (vec-y v)))])
    (if (> longest 0)
        (let ([nx (/ (vec-x v) longest)]
              [ny (/ (vec-y v) longest)])
          (vec (* (ceiling (abs nx)) (sign nx))
               (* (ceiling (abs ny)) (sign ny))))
        v)))

(define (parse input)
  (map (lambda (line) (string-split line " "))
       (string-split (file->string input) "\n")))

(define (normalize-motions motions)
  (if (empty? motions)
      '()
      (match (first motions)
        [`(,dir ,n)
         (append (repeat (string->number n) `(,dir 1)) (normalize-motions (rest motions)))])))

(define (touching? t h)
  (let* ([dx (- (vec-x h) (vec-x t))]
         [dy (- (vec-y h) (vec-y t))]
         [dist (sqrt (+ (* dx dx) (* dy dy)))])
    (< dist 2)))

(define (next-tail t h)
  (if (touching? t h)
      t
      (let* ([dx (- (vec-x h) (vec-x t))]
             [dy (- (vec-y h) (vec-y t))]
             [nt (vec-add t (normalize (vec dx dy)))])
        nt)))

(define (next-head h motion)
  (match motion
    [`("R" ,n) (vec-add h (vec n 0))]
    [`("L" ,n) (vec-add h (vec (- n) 0))]
    [`("U" ,n) (vec-add h (vec 0 n))]
    [`("D" ,n) (vec-add h (vec 0 (- n)))]))

(define (solution input)
  (define h (vec 0 0))
  (define t (vec 0 0))
  (define visited (make-hash))

  (for ([motion (normalize-motions (parse input))])
    (set! h (next-head h motion))
    (set! t (next-tail t h))
    (hash-set! visited `(,(vec-x t) ,(vec-y t)) #t))

  (hash-count visited))

(check-equal? (solution "test/1") 13)

(solution "input")
