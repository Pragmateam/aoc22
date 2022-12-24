#lang racket

(require racket/file
         racket/string
         rackunit)

(struct vec (x y) #:transparent)

; Borrowed from day 22 😁
(define (parse input)
  (define x 0)
  (define y 0)

  (define (char!) (set! x (add1 x)))
  (define (line!) (set! x 0) (set! y (add1 y)))

  (define elves '())

  (for ([c (file->string input)])
    (match c
      [#\space (char!)]
      [#\. (char!)]
      [#\# (set! elves (cons (vec x y) elves)) (char!)]
      [#\newline (line!)]))

  elves)

(define (vec+ a b)
  (vec (+ (vec-x a) (vec-x b))
       (+ (vec-y a) (vec-y b))))

(define (pos->pos~ elves p r)
  (define movements (list
                     (list (vec 0 -1) (vec 1 -1) (vec -1 -1))
                     (list (vec 0 1) (vec 1 1) (vec -1 1))
                     (list (vec -1 0) (vec -1 1) (vec -1 -1))
                     (list (vec 1 0) (vec 1 1) (vec 1 -1))))

  (define (all-empty? coords)
    (andmap (λ (e) (not (set-member? elves e))) (map (λ (mov) (vec+ p mov)) coords)))

  (define (rmod n)
    (modulo (+ r n) (length movements)))

  (cond
    [(all-empty? (remove-duplicates (flatten movements)))
     p]
    [(all-empty? (list-ref movements (rmod 0)))
     (vec+ p (first (list-ref movements (rmod 0))))
     ]
    [(all-empty? (list-ref movements (rmod 1)))
     (vec+ p (first (list-ref movements (rmod 1))))
     ]
    [(all-empty? (list-ref movements (rmod 2)))
     (vec+ p (first (list-ref movements (rmod 2))))
     ]
    [(all-empty? (list-ref movements (rmod 3)))
     (vec+ p (first (list-ref movements (rmod 3))))
     ]))

(define (bounding-box-dims elves)
  (let ([x (map vec-x elves)]
        [y (map vec-y elves)])
    (values (add1 (- (apply max x) (apply min x)))
            (add1 (- (apply max y) (apply min y))))))

(define (move elves r)
  (define proposals (make-hash))
  (define collision (make-hash))

  (for ([elf elves])
    (let ([pos~ (pos->pos~ (list->set elves) elf r)])
      (hash-set! proposals elf pos~)
      (hash-update! collision pos~ add1 0)))

  (map (λ (elf)
         (let ([proposal (hash-ref proposals elf)])
           (if (> (hash-ref collision proposal) 1)
               elf
               proposal)))
       elves))

(define (display-elves elves)
  (define w 10)
  (define h 10)

  (for ([y (range h)])
    (for ([x (range w)])
      (let ([point (vec x y)])
        (match (set-member? elves point)
          [#f (display ".")]
          [#t (display "#")])))
    (display "\n")))

(define (solution input)
  (define elves (parse input))

  (for ([i (range 10)])
    (set! elves (move elves i)))

  (let-values ([(w h) (bounding-box-dims elves)])
    (- (* w h) (length elves))))

(check-equal? (solution "test/1") 110)

(solution "input")
