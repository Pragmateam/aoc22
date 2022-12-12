#lang racket

(require racket/file
         racket/set
         rackunit)

(struct point (x y cost prev) #:transparent)
(define (point->list p) `(,(point-x p) ,(point-y p)))
(define (point-distance a b)
  (let ([dx (- (point-x a) (point-x b))]
        [dy (- (point-y a) (point-y b))])
    (sqrt (+ (* dx dx) (* dy dy)))))
(define (point-move p dir)
  (struct-copy point p
               (x (+ (point-x p) (first dir)))
               (y (+ (point-y p) (second dir)))
               (cost (add1 (point-cost p)))
               (prev p)))

(define (hmap-height hmap p)
  (list-ref (list-ref hmap (point-y p)) (point-x p)))

(define (a*-path p (path '()))
  (define next-path (cons (point->list p) path))
  (if (eq? (point-prev p) '())
      next-path
      (a*-path (point-prev p) next-path)))

(define (a*-neighbours hmap p)
  (define w (length (first hmap)))
  (define h (length hmap))
  (define dirs '((0 1) (1 0) (-1 0) (0 -1)))
  (define (inside? p w h)
    (and (>= (point-x p) 0)
         (>= (point-y p) 0)
         (< (point-x p) w)
         (< (point-y p) h)))

  (filter (compose not void?)
          (for/list ([dir dirs])
            (let ([neighbour (point-move p dir)])
              (when (inside? neighbour w h)
                neighbour)))))

(define (a*-next hmap seen pos dst)
  (define (valid? p)
    (and (not (set-member? seen (point->list p)))
         (>= (- (hmap-height hmap pos) (hmap-height hmap p)) -1)))
  (filter valid? (a*-neighbours hmap pos)))

(define (a*-best hmap points dst)
  (define (cost p)
    (list (+ 1 (point-cost p) (point-distance p dst)) p))
  (let ([available (sort (map cost points)
                         (λ (a b) (< (first a) (first b))))])
    (map second available)))

(define (a* hmap seen ridge dst)
  (if (empty? ridge)
      '()
      (let* ([best (a*-best hmap ridge dst)]
             [current (first best)]
             [next (a*-next hmap seen current dst)]
             [next-ridge (remove-duplicates (append (rest best) next) #:key point->list)])
        (cond
          [(equal? (point->list current) (point->list dst)) (a*-path current)]
          [else
           (a* hmap (set-add seen (point->list current)) next-ridge dst)]))))

(define (parse input)
  (define starts '())
  (define end (void))
  (define hmap (map string->list (string-split (file->string input) "\n")))
  (define w (length (first hmap)))
  (define h (length hmap))
  (define processed-hmap (for/list ([y (range h)])
                           (for/list ([x (range w)])
                             (match (list-ref (list-ref hmap y) x)
                               [#\S (set! starts (cons (point x y 0 '()) starts)) (char->integer #\a)]
                               [#\a (set! starts (cons (point x y 0 '()) starts)) (char->integer #\a)]
                               [#\E (set! end (point x y 0 '())) (char->integer #\z)]
                               [letter (char->integer letter)]))))

  (values processed-hmap starts end))

(define (solution input)
  (define-values (hmap starts end) (parse input))
  (apply min
         (map (compose sub1 length)
              (filter (compose not empty?)
                      (for/list ([start starts])
                        (a* hmap (set) (list start) end))))))

(check-equal? (solution "test/1") 29)

(solution "input")
