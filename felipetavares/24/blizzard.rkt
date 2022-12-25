#lang racket

(require racket/file
         rackunit)

(struct vec (x y) #:transparent)

(define (vec+ a b)
  (vec (+ (vec-x a) (vec-x b))
       (+ (vec-y a) (vec-y b))))

; This code is now standard it seems 🤣
(define (parse input)
  (define x 0)
  (define y 0)

  (define (char!) (set! x (add1 x)))
  (define (line!) (set! x 0) (set! y (add1 y)))

  (define blizzards '())
  (define walls '())
  (define (blizz b)
    (set! blizzards (cons (cons (vec x y) b) blizzards))
    (char!))

  (for ([c (file->string input)])
    (match c
      [#\space (char!)]
      [#\. (char!)]
      [#\# (set! walls (cons (vec x y) walls)) (char!)]
      [#\> (blizz (vec +1 0))]
      [#\< (blizz (vec -1 0))]
      [#\v (blizz (vec 0 +1))]
      [#\^ (blizz (vec 0 -1))]
      [#\newline (line!)]))

  (values blizzards walls))

(define (entrypoints walls)
  (define w (add1 (apply max (map vec-x walls))))
  (define h (add1 (apply max (map vec-y walls))))

  ; Find the ground tiles
  (define ground (sort (set->list
                        (set-subtract
                         (list->set (map (λ (coords) (apply vec coords))
                                         (cartesian-product (range w) (range h))))
                         (list->set walls)))
                       < #:key vec-y))

  (values (first ground) (last ground) w h))

(define (vec+% a b w h)
  (vec (add1 (modulo (sub1 (+ (vec-x a) (vec-x b))) (- w 2)))
       (add1 (modulo (sub1 (+ (vec-y a) (vec-y b))) (- h 2)))))

(define (move-blizzards blizzards w h)
  (define (move-blizzard b)
    (match-define (cons pos dir) b)
    (define pos~ (vec+% pos dir w h))

    (cons pos~ dir))

  (map move-blizzard blizzards))

(define (display-map blizzards walls w h)
  (for ([y (range h)])
    (for ([x (range w)])
      (let ([point (vec x y)])
        (cond
          [(set-member? blizzards point) (display "@")]
          [(set-member? walls point) (display "#")]
          [else (display ".")])))
    (display "\n")))

(define (shortest-time walls blizzards w h lm end pos (t 0) (cache (make-hash)))
  (if (or (equal? pos end) (>= t 320))
      t
      (if (hash-has-key? cache (list pos (modulo t lm)))
          (+ t (hash-ref cache (list pos (modulo t lm))))
          (let* ([blizzards-set (list->set (map car blizzards))]
                 [best (apply min
                              (cons
                               100000
                               ; Move
                               (filter (compose not void?)
                                       (for/list ([d (list (vec 0 0) (vec +1 0) (vec -1 0) (vec 0 +1) (vec 0 -1))])
                                         (let ([pos~ (vec+ pos d)])
                                           (when (not (or (set-member? blizzards-set pos~)
                                                          (set-member? walls pos~)
                                                          (< (vec-y pos~) 0)))
                                             (shortest-time walls (move-blizzards blizzards w h) w h lm end pos~ (add1 t) cache)))))))])
            ; (display-map blizzards-set walls w h)
            (hash-set! cache (list pos (modulo t lm)) (- best t))
            (when (= (modulo (hash-count cache) 10000) 0)
              (displayln (hash-count cache)))
            best))))

(define (solution input)
  (define-values (blizzards walls) (parse input))
  (define-values (start end w h) (entrypoints walls))

  ; Debug blizzard movement
  ; (display-map (list->set (map car blizzards)) (list->set walls) w h)
  ; (display "\n")

  (shortest-time (list->set walls) (move-blizzards blizzards w h) w h (lcm (- w 2) (- h 2)) end start))

(check-equal? (solution "test/1") 18)

(solution "input")
