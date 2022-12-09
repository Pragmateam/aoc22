#lang racket

(require racket/string)
(require racket/file)
(require algorithms)
(require rackunit)

(struct vec (x y))
(define (sign n) (match (positive? n) [#t 1] [#f -1]))
(define (vec-add a b) (vec (+ (vec-x a) (vec-x b)) (+ (vec-y a) (vec-y b))))
(define (vec-norm v) ; not your garden-variety norm
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
             [dy (- (vec-y h) (vec-y t))])
        (vec-add t (vec-norm (vec dx dy))))))

(define (next-head h motion)
  (match motion
    [`("R" ,n) (vec-add h (vec n 0))]
    [`("L" ,n) (vec-add h (vec (- n) 0))]
    [`("U" ,n) (vec-add h (vec 0 n))]
    [`("D" ,n) (vec-add h (vec 0 (- n)))]))

(define (solution input)
  (define rope (repeat 10 (vec 0 0)))
  (define visited (make-hash))

  (for ([motion (normalize-motions (parse input))])
    (set! rope (list-set rope 0 (next-head (list-ref rope 0) motion)))
    (for ([i (range 1 (length rope))])
      (set! rope (list-set rope i (next-tail (list-ref rope i) (list-ref rope (- i 1))))))
    (define t (list-ref rope 9))
    (hash-set! visited `(,(vec-x t) ,(vec-y t)) #t))

  (hash-count visited))

(check-equal? (solution "test/1") 1)
(check-equal? (solution "test/2") 36)

(solution "input")
