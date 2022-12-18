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
    (display (format "~a - " y))
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

; Did some manual work using the
; commented code below. It prints
; the period of the pattern and the
; corresponding height growth for
; each period. Also prints the first
; repeating point. Using that, I
; calculated the final value with
; bc:
;
;(1000000000000-1820)/1740
; 574712642
; 574712642 * 2759 + 2893
; 1585632182171
; 4637-2893
; 1744
; 1585632182171+1744
; 1585632183915
;
; Repeating at 50 (h = 78) with a height of 53; repeating every 35
; Repeating at 1820 (h = 2893) with a height of 2759; repeating every 1740
; 1585632182171 + ??
; Repeating at 2920 (h = 4637) with a height of 2759; repeating every 1740
;
; (define seen (make-hash))
(define (cycles tower rocks pattern max-n (t 0) (n 0))
  ; (let ([pos (cons (modulo n (vector-length rocks))
  ;                  (modulo t (vector-length pattern)))])
  ;   (if (hash-has-key? seen pos)
  ;       (displayln (format "Repeating at ~a (h = ~a) with a height of ~a; repeating every ~a"
  ;                          n
  ;                          (highest tower)
  ;                          (- (highest tower)
  ;                             (car (hash-ref seen pos)))
  ;                          (- n
  ;                             (cdr (hash-ref seen pos)))))
  ;       (hash-set! seen pos (cons (highest tower) n))))

  (if (>= n max-n)
      tower
      (let-values ([(tower~ t~) (drop-rock tower (place-rock tower rocks n) pattern t)])
        (cycles tower~ rocks pattern max-n t~ (add1 n)))))

(define (solution input)
  (define pattern (parse input))
  (define tower (cycles (set) rocks pattern 3000))

  (displayln (vector-length pattern))

  (draw-tower tower 100))

(check-equal? (solution "test/1") 1514285714288)

(solution "input")
