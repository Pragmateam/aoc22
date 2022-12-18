#lang racket

(require racket/file
         racket/string
         rackunit)

(struct vec2 (x y) #:transparent)

(define (vec2-add a b)
  (match (cons a b) [(cons (vec2 ax ay) (vec2 bx by)) (vec2 (+ ax bx) (+ ay by))]))

(define rocks
  (vector
   (set (vec2 0 0) (vec2 1 0) (vec2 2 0) (vec2 3 0))
   (set (vec2 1 2) (vec2 1 1) (vec2 1 0) (vec2 0 1) (vec2 2 1))
   (set (vec2 2 2) (vec2 2 1) (vec2 2 0) (vec2 1 0) (vec2 0 0))
   (set (vec2 0 0) (vec2 0 1) (vec2 0 2) (vec2 0 3))
   (set (vec2 0 0) (vec2 0 1) (vec2 1 0) (vec2 1 1))))

(define (parse input)
  (list->vector (map (compose list->string list) (string->list (string-trim (file->string input))))))

(define (rock-move rock vec)
  (list->set (map (λ (rock-vec) (vec2-add rock-vec vec)) (set->list rock))))

(define (in-floor-or-walls? rock)
  (define left (apply min (map vec2-x (set->list rock))))
  (define right (apply max (map vec2-x (set->list rock))))
  (define bottom (apply min (map vec2-y (set->list rock))))

  (or (< left 0) (>= right 7) (< bottom 0)))

(define (draw-tower tower height)
  (for ([y (inclusive-range (sub1 height) 0 -1)])
    (for ([x (range 7)])
      (if (set-member? tower (vec2 x y))
          (display "#")
          (display ".")))
    (displayln "")))

(define (move tower rock movement)
  (match movement
    [(cons direction stop?)
     (let ([tentative-rock (rock-move rock direction)])
       (if (or (> (set-count (set-intersect tentative-rock tower)) 0)
               (in-floor-or-walls? tentative-rock))
           (values (if stop? (set-union tower rock) tower) rock stop?)
           (values tower tentative-rock #f)))]))

(define (drop-rock tower rock pattern t)
  (define movement (match (vector-ref pattern (modulo t (vector-length pattern)))
                     [">" (vec2 1 0)] ["<" (vec2 -1 0)]))

  (let-values ([(tower~1 rock~1 _) (move tower rock (cons movement #f))])
    (let-values ([(tower~2 rock~2 stop?) (move tower~1 rock~1 (cons (vec2 0 -1) #t))])
      (if stop?
          (values tower~2 (add1 t))
          (drop-rock tower~2 rock~2 pattern (add1 t))))))

(define (highest tower)
  (apply max (cons 0 (map (compose add1 vec2-y) (set->list tower)))))

(define (place-rock tower rocks n)
  (rock-move (vector-ref rocks (modulo n (vector-length rocks)))
             (vec2 2 (+ (highest tower) 3))))

(define (cycles tower rocks pattern max-n (t 0) (n 0))
  (if (>= n max-n)
      tower
      (let-values ([(tower~ t~) (drop-rock tower (place-rock tower rocks n) pattern t)])
        (cycles tower~ rocks pattern max-n t~ (add1 n)))))

(define (solution input)
  (define pattern (parse input))

  (highest (cycles (set) rocks pattern 2022)))

(check-equal? (solution "test/1") 3068)

(solution "input")
