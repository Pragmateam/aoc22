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
  (and (>= (second r1) (first r2)) (<= (first r1) (second r2))))

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
      (join-all (join-ranges (rest rs) (first rs)))))

(define (solution input y)
  (define circles (parse input))
  (define beacons-on-y (set))

  (- (apply +
            (map (λ (r) (add1 (- (second r) (first r))))
                 (join-all
                  (filter (compose not void?)
                          (for/list ([circle circles])
                            (let* ([d (vec2->manhattan (vec2-sub (first circle) (second circle)))]
                                   [bx (vec2-x (first circle))]
                                   [by (- y (vec2-y (first circle)))]
                                   [xmax (+ bx (solve-max d by))]
                                   [xmin (+ bx (solve-min d by))])
                              (when (> d (abs by))
                                (when (= y (vec2-y (second circle))) (set! beacons-on-y (set-add beacons-on-y (second circle))))
                                (list xmin xmax))))))))
     (set-count beacons-on-y)))

(check-equal? (solution "test/1" 10) 26)

(solution "input" 2000000)
