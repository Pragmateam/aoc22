#lang racket

(require racket/file
         racket/string
         rackunit)

(struct vec (x y) #:transparent)
(struct actor (pos facing) #:transparent)

; Some directions
(define up (vec 0 -1))
(define down (vec 0 1))
(define left (vec -1 0))
(define right (vec 1 0))

; Useful for rotations
(define dir->num (make-hash `((,right . 0)
                              (,down . 1)
                              (,left . 2)
                              (,up . 3))))

(define num->dir (make-hash `((0 . ,right)
                              (1 . ,down)
                              (2 . ,left)
                              (3 . ,up))))

(define (vec+ a b)
  (vec (+ (vec-x a) (vec-x b))
       (+ (vec-y a) (vec-y b))))

(define (parse input)
  (define (parse-map str)
    (define x 0)
    (define y 0)

    (define (char!) (set! x (add1 x)))
    (define (line!) (set! x 0) (set! y (add1 y)))

    (define open '())
    (define solid '())

    (for ([c str])
      (match c
        [#\space (char!)]
        [#\. (set! open (cons (vec x y) open)) (char!)]
        [#\# (set! solid (cons (vec x y) solid)) (char!)]
        [#\newline (line!)]))

    (cons (list->set open) (list->set solid)))

  (define (parse-path str)
    (map (λ (m) (match (string->number m) [#f (string->symbol m)] [n n]))
         (regexp-match* #rx"[0-9]+|(L|R)" str)))

  (match-let* ([`(,map-str ,path-str) (string-split (file->string input) "\n\n")]
               [m (parse-map map-str)])
    (values (car m) (cdr m) (parse-path path-str))))

(define (draw-map open solid w h)
  (for ([y (range h)])
    (for ([x (range w)])
      (let ([point (vec x y)])
        (match (cons (set-member? open point) (set-member? solid point))
          [(cons #t #f) (display ".")]
          [(cons #f #t) (display "#")]
          [(cons #f #f) (display " ")])))
    (display "\n")))

(define (bounding-box positions)
  (let ([pos-list (set->list positions)])
    (values (add1 (apply max (map vec-x pos-list)))
            (add1 (apply max (map vec-y pos-list))))))

(define (column-min-max positions col height)
  (define min-y 0)
  (define max-y 0)

  (for ([y (range height)])
    #:final (set-member? positions (vec col y))
    (set! min-y y))

  (for ([y (reverse (range height))])
    #:final (set-member? positions (vec col y))
    (set! max-y y))

  (values min-y max-y))

(define (row-min-max positions row width)
  (define min-x 0)
  (define max-x 0)

  (for ([x (range width)])
    #:final (set-member? positions (vec x row))
    (set! min-x x))

  (for ([x (reverse (range width))])
    #:final (set-member? positions (vec x row))
    (set! max-x x))

  (values min-x max-x))

(define (wormholes positions)
  (define-values (w h) (bounding-box positions))
  (define wormholes (make-hash))

  ; For each column
  (for ([x (range w)])
    (let-values ([(min-y max-y) (column-min-max positions x h)])
      ; Going up
      (hash-set! wormholes (cons (vec x min-y) up) (vec x max-y))
      ; Going down
      (hash-set! wormholes (cons (vec x max-y) down) (vec x min-y))))

  ; For each row
  (for ([y (range h)])
    (let-values ([(min-x max-x) (row-min-max positions y w)])
      ; Going left
      (hash-set! wormholes (cons (vec min-x y) left) (vec max-x y))
      ; Going right
      (hash-set! wormholes (cons (vec max-x y) right) (vec min-x y))))

  wormholes)

(define (repeat n fn arg)
  (if (= n 0)
      arg
      (repeat (sub1 n) fn (fn arg))))

(define (position->position~ portals solid pos dir)
  (let ([pos~ (hash-ref portals (cons pos dir) (vec+ pos dir))])
    (if (set-member? solid pos~)
        pos
        pos~)))

(define (actor->actor~ portals solid instruction act)
  (match instruction
    ; Walk a single step
    [1
     (actor
      (position->position~ portals solid (actor-pos act) (actor-facing act))
      (actor-facing act))]
    ; Walk multiple steps by breaking it down into single steps
    [(? number? steps)
     (repeat steps
             (λ (a) (actor->actor~ portals solid 1 a))
             act)]
    ['L
     (actor (actor-pos act)
            (hash-ref num->dir (modulo (sub1 (hash-ref dir->num (actor-facing act))) 4)))]
    ['R
     (actor (actor-pos act)
            (hash-ref num->dir (modulo (add1 (hash-ref dir->num (actor-facing act))) 4)))]))

(define (solution input)
  (define-values (open solid path) (parse input))

  (define all-tiles (set-union open solid))
  (define portals (wormholes all-tiles))

  (define (leftmost pos-a pos-b) (< (vec-x pos-a) (vec-x pos-b)))
  (define (top-row pos) (= (vec-y pos) 0))
  (define beginning (first (sort (filter top-row (set->list open)) leftmost)))

  (define act (actor beginning right))

  ; Debug the map reading code
  ; (let-values ([(w h) (bounding-box all-tiles)])
  ;   (draw-map open solid w h))

  (for ([instruction path])
    (set! act (actor->actor~ portals solid instruction act)))

  (+
   (* 1000 (add1 (vec-y (actor-pos act))))
   (* 4 (add1 (vec-x (actor-pos act))))
   (hash-ref dir->num (actor-facing act))))

(check-equal? (solution "test/1") 6032)

(solution "input")
