#lang racket

(require racket/file
         racket/string
         rackunit)

(struct vec2 (x y) #:transparent)

(define (vec2-sub a b)
  (vec2 (- (vec2-x a) (vec2-x b))
        (- (vec2-y a) (vec2-y b))))

(define (vec2->manhattan v)
  (+ (abs (vec2-x v)) (abs (vec2-y v))))

(define (parse input)
  (map (λ (numbers) (match numbers [`(,sx ,sy ,bx ,by) (list (vec2 sx sy) (vec2 bx by))]))
       (map (λ (line) (map string->number (regexp-match* #px"-?[0-9]+" line)))
            (string-split (file->string input) "\n"))))

(define (solve-max d y)
  (- d (abs y)))

(define (solve-min d y)
  (- (abs y) d))

(define (overlap? r1 r2)
  (and (>= (second r1) (sub1 (first r2))) (<= (first r1) (add1 (second r2)))))

(define (join-range r1 r2)
  (if (or (overlap? r1 r2) (overlap? r2 r1))
      `((,(apply min (append r1 r2)) ,(apply max (append r1 r2))))
      `(,r1 ,r2)))

(define (join-ranges rs r)
  (if (empty? rs)
      (list r)
      (let ([joined (join-range (first rs) r)])
        (if (= 1 (length joined))
            (join-ranges (rest rs) (first joined))
            (cons (first rs) (join-ranges (rest rs) r))))))

(define (join-all rs)
  (if (= 1 (length rs))
      rs
      (let ([new-ranges (join-ranges (rest rs) (first rs))])
        (if (= (length rs) (length new-ranges))
            rs
            (join-all new-ranges)))))

(define (top-range ranges)
  (first (sort ranges (lambda (a b) (> (first a) (first b))))))

(define (solution input max-y)
  (define circles (parse input))

  (define y (apply +
                   (for/list ([y (inclusive-range 0 max-y)])
                     (if (= 1 (length (join-all
                                       (filter (compose not void?)
                                               (for/list ([circle circles])
                                                 (let* ([d (vec2->manhattan (vec2-sub (first circle) (second circle)))]
                                                        [bx (vec2-x (first circle))]
                                                        [by (- y (vec2-y (first circle)))]
                                                        [xmax (+ bx (solve-max d by))]
                                                        [xmin (+ bx (solve-min d by))])
                                                   (when (> d (abs by))
                                                     (list xmin xmax))))))))
                         0
                         y))))

  (+ y (* 4000000 (sub1 (first (top-range (join-all
                               (filter (compose not void?)
                                       (for/list ([circle circles])
                                         (let* ([d (vec2->manhattan (vec2-sub (first circle) (second circle)))]
                                                [bx (vec2-x (first circle))]
                                                [by (- y (vec2-y (first circle)))]
                                                [xmax (+ bx (solve-max d by))]
                                                [xmin (+ bx (solve-min d by))])
                                           (when (> d (abs by))
                                             (list xmin xmax))))))))))))

(check-equal? (solution "test/1" 20) 56000011)

(solution "input" 4000000)
