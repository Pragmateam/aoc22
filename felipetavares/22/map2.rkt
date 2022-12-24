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

(define (vec* v scale)
  (vec (* (vec-x v) scale)
       (* (vec-y v) scale)))

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

(define (draw-map-and-actor open solid act w h)
  (displayln "---")
  (for ([y (range h)])
    (for ([x (range w)])
      (let ([point (vec x y)])
        (match (list (set-member? open point) (set-member? solid point) (equal? (actor-pos act) point))
          [(list _ _ #t)
           (cond
             [(equal? (actor-facing act) up) (display "^")]
             [(equal? (actor-facing act) down) (display "v")]
             [(equal? (actor-facing act) left) (display "<")]
             [(equal? (actor-facing act) right) (display ">")])]
          [(list #t #f _) (display ".")]
          [(list #f #t _) (display "#")]
          [(list #f #f _) (display " ")])))
    (display "\n"))
  (displayln "---"))

(define (draw-map-and-portals open solid portals w h)
  (displayln "---")
  (for ([y (range h)])
    (for ([x (range w)])
      (let ([point (vec x y)])
        (match (list (set-member? open point)
                     (set-member? solid point)
                     (or (hash-has-key? portals (cons point up))
                         (hash-has-key? portals (cons point down))
                         (hash-has-key? portals (cons point left))
                         (hash-has-key? portals (cons point right))))
          [(list _ _ #t) (display "@")]
          [(list #t #f _) (display ".")]
          [(list #f #t _) (display "#")]
          [(list #f #f _) (display " ")])))
    (display "\n"))
  (displayln "---"))


(define (bounding-box positions)
  (let ([pos-list (set->list positions)])
    (values (add1 (apply max (map vec-x pos-list)))
            (add1 (apply max (map vec-y pos-list))))))

(define (rotate n v)
  (hash-ref num->dir (modulo (+ n (hash-ref dir->num v)) 4)))

(define (repeat n fn arg)
  (if (= n 0)
      arg
      (repeat (sub1 n) fn (fn arg))))

(define (position->position~ portals solid pos dir)
  (let ([pos~ (hash-ref portals (cons pos dir) (cons (vec+ pos dir) dir))])
    (if (set-member? solid (car pos~))
        (cons pos dir)
        pos~)))

(define (actor->actor~ portals solid instruction act)
  (match instruction
    ; Walk a single step
    [1
     (let ([pos~ (position->position~ portals solid (actor-pos act) (actor-facing act))])
       (actor
        (car pos~)
        (cdr pos~)))]
    ; Walk multiple steps by breaking it down into single steps
    [(? number? steps)
     (repeat steps
             (λ (a) (actor->actor~ portals solid 1 a))
             act)]
    ['L
     (actor (actor-pos act)
            (rotate -1 (actor-facing act)))]
    ['R
     (actor (actor-pos act)
            (rotate +1 (actor-facing act)))]))

(define (build-facemap all-tiles)
  (define-values (w h) (bounding-box all-tiles))
  (define side (gcd w h))

  ; Scale the cubemap down by the size of the cube side
  (list->set (map (λ (tile) (vec (quotient (vec-x tile) side)
                                 (quotient (vec-y tile) side)))
                  (set->list all-tiles))))

(define (find-connection facemap face dist direction (visited (set)))
  (if (and (set-member? facemap face)
           (not (set-member? visited face)))
      (if (= dist 0)
          ; Find the damn connections
          (if (not (set-member? facemap (vec+ face direction)))
              (list (cons face direction))
              '())
          ; Recurse
          (apply append
                 (for/list ([neighboring-dir (list up down left right)])
                   (find-connection
                    facemap
                    (vec+ face neighboring-dir)
                    (sub1 dist)
                    direction
                    (set-add visited face)))))
      '()))

(define (match-faces all-tiles)
  (define facemap (build-facemap all-tiles))
  (define-values (w h) (bounding-box facemap))
  (define connected (make-hash))

  ; Show the scaled down faces
  ; (draw-map (set) facemap w h)

  ; Add connections for d = 2
  (for ([y (range h)])
    (for ([x (range w)])
      (let ([p (vec x y)])
        (match (list (set-member? facemap p)
                     (set-member? facemap (vec+ p right))
                     (set-member? facemap (vec+ p up))
                     (set-member? facemap (vec+ p left))
                     (set-member? facemap (vec+ p down)))
          ; Connect nearby faces
          ['(#f #t #t #f #f)
           ; Connect an edge to another
           (hash-set! connected
                      (cons (vec+ p right) left)
                      (cons (vec+ p up) down))
           (hash-set! connected
                      (cons (vec+ p up) down)
                      (cons (vec+ p right) left))
           ]
          ['(#f #f #t #t #f)
           (hash-set! connected
                      (cons (vec+ p up) down)
                      (cons (vec+ p left) right))
           (hash-set! connected
                      (cons (vec+ p left) right)
                      (cons (vec+ p up) down))]
          ['(#f #f #f #t #t)
           (hash-set! connected
                      (cons (vec+ p left) right)
                      (cons (vec+ p down) up))
           (hash-set! connected
                      (cons (vec+ p down) up)
                      (cons (vec+ p left) right))]
          ['(#f #t #f #f #t)
           (hash-set! connected
                      (cons (vec+ p down) up)
                      (cons (vec+ p right) left))
           (hash-set! connected
                      (cons (vec+ p right) left)
                      (cons (vec+ p down) up))]
          [_ (void)]))))

  ; Add connections for d = 3
  (for ([y (range h)])
    (for ([x (range w)])
      (let ([p (vec x y)])
        (when (set-member? facemap p)
          (let ([my-edges (filter (compose not void?)
                                  (for/list ([dir (list right up left down)])
                                    ; Check if edge is free
                                    (when (and (not (set-member? facemap (vec+ p dir)))
                                               (not (hash-has-key? connected (cons p dir))))
                                      (cons p dir))
                                    ))])
            (for ([edge my-edges])
              (for ([connection (find-connection facemap p 3 (cdr edge))])
                (when (not (or (hash-has-key? connected edge)
                               (hash-has-key? connected connection)))
                  (hash-set! connected edge connection)
                  (hash-set! connected connection edge)))))))))

  ; Add connections for d = 4
  (for ([y (range h)])
    (for ([x (range w)])
      (let ([p (vec x y)])
        (when (set-member? facemap p)
          (let ([my-edges (filter (compose not void?)
                                  (for/list ([dir (list right up left down)])
                                    ; Check if edge is free
                                    (when (and (not (set-member? facemap (vec+ p dir)))
                                               (not (hash-has-key? connected (cons p dir))))
                                      (cons p dir))
                                    ))])
            (for ([edge my-edges])
              (for ([connection (find-connection facemap p 4 (rotate 1 (cdr edge)))])
                (when (not (or (hash-has-key? connected edge)
                               (hash-has-key? connected connection)))
                  (hash-set! connected edge connection)
                  (hash-set! connected connection edge)))
              (for ([connection (find-connection facemap p 4 (rotate -1 (cdr edge)))])
                (when (not (or (hash-has-key? connected edge)
                               (hash-has-key? connected connection)))
                  (hash-set! connected edge connection)
                  (hash-set! connected connection edge)))))))))

  ; Add connections for d = 5
  (for ([y (range h)])
    (for ([x (range w)])
      (let ([p (vec x y)])
        (when (set-member? facemap p)
          (let ([my-edges (filter (compose not void?)
                                  (for/list ([dir (list right up left down)])
                                    ; Check if edge is free
                                    (when (and (not (set-member? facemap (vec+ p dir)))
                                               (not (hash-has-key? connected (cons p dir))))
                                      (cons p dir))
                                    ))])
            (for ([edge my-edges])
              (for ([connection (find-connection facemap p 5 (rotate 2 (cdr edge)))])
                (when (not (or (hash-has-key? connected edge)
                               (hash-has-key? connected connection)))
                  (hash-set! connected edge connection)
                  (hash-set! connected connection edge)))))))))

  connected)

(define (generate-wormhole-coordinates start dir len)
  ; Calculate the center of the tile in unit coords
  (define center (vec+ start (vec 1/2 1/2)))
  ; Calculate the edge
  (define edge-center (vec+ center (vec* dir 1/2)))

  ; Find the start position of the edge
  (define tp (vec* (vec+ edge-center (vec* (rotate +1 dir) 1/2)) len))

  (define p
    (cond
      [(equal? dir right) (vec+ tp (vec -1 -1))]
      [(equal? dir up) (vec+ tp (vec -1 0))]
      [(equal? dir down) (vec+ tp (vec 0 -1))]
      [else tp]))

  ; Walk in the opposite direction for a whole side
  (for/list ([_ (range len)])
    (let ([coord p])
      (set! p (vec+ p (rotate -1 dir)))
      (cons coord dir))))

(define (connections->wormholes connections side)
  (define portals (make-hash))

  (for ([conn (hash->list connections)])
    (match conn
      [(cons (cons pa da) (cons pb db))
       (for ([coord-a (generate-wormhole-coordinates pa da side)]
             [coord-b (reverse (generate-wormhole-coordinates pb db side))])
         ; Link the two points together
         (hash-set! portals coord-a (cons (car coord-b) (rotate 2 (cdr coord-b))))
         (hash-set! portals coord-b (cons (car coord-a) (rotate 2 (cdr coord-a))))
         )]))

  portals)

(define (solution input)
  (define-values (open solid path) (parse input))

  (define all-tiles (set-union open solid))

  (define (leftmost pos-a pos-b) (< (vec-x pos-a) (vec-x pos-b)))
  (define (top-row pos) (= (vec-y pos) 0))
  (define beginning (first (sort (filter top-row (set->list open)) leftmost)))

  (define act (actor beginning right))

  (define-values (w h) (bounding-box all-tiles))
  (define portals (connections->wormholes (match-faces all-tiles) (gcd w h)))

  ; (draw-map-and-portals open solid portals w h)

  ; Calculate the paths
  (for ([instruction path])
    (set! act (actor->actor~ portals solid instruction act))
    ; (draw-map-and-actor open solid act w h)
    )

  (+
   (* 1000 (add1 (vec-y (actor-pos act))))
   (* 4 (add1 (vec-x (actor-pos act))))
   (hash-ref dir->num (actor-facing act))))

(check-equal? (solution "test/1") 5031)

(solution "input")
