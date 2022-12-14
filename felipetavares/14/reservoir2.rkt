#lang racket

(require racket/file
         racket/string
         rackunit)

(struct vec2 (x y) #:transparent)

(define below (vec2 0 1))
(define below-left (vec2 -1 1))
(define below-right (vec2 1 1))

(define (vec2-add a b)
  (vec2 (+ (vec2-x a) (vec2-x b))
        (+ (vec2-y a) (vec2-y b))))

(define (dir a b)
  (let ([delta (- a b)])
    (/ delta (abs delta))))

(define (free? env p threshold)
  (not (or (= (vec2-y p) threshold) (hash-has-key? env p))))

(define (move env p p~)
  (hash-set (hash-remove env p) p~ 'o))

(define (p->p~ env p threshold)
  (let ([b (vec2-add p below)]
        [bl (vec2-add p below-left)]
        [br (vec2-add p below-right)])
    (cond
      [(free? env b threshold) b]
      [(free? env bl threshold) bl]
      [(free? env br threshold) br]
      [else
       (if (equal? p (vec2 500 0))
           'blocking
           'rest)])))

(define (simulate env p threshold)
  (let ([p~ (p->p~ env p threshold)])
    (match p~
      ['rest env]
      ['blocking 'done]
      [p~ (simulate (move env p p~) p~ threshold)])))

(define (parse input)
  (define (string->vec2 str)
    (match (map string->number (string-split (string-trim str) ","))
      [`(,x ,y) (vec2 x y)]))
  (define (parse-line line)
    (map string->vec2 (string-split line "->")))

  (map parse-line
       (string-split (file->string input) "\n")))

(define (build-env lines)
  (define env (hash))

  (for ([line lines])
    (for ([i (range 1 (length line))])
      (let ([cur (list-ref line i)]
            [prev (list-ref line (sub1 i))])
        (if (= (vec2-x prev) (vec2-x cur))
            (for ([y (inclusive-range (vec2-y prev)
                                      (vec2-y cur)
                                      (dir (vec2-y cur) (vec2-y prev)))])
              (set! env (hash-set env (vec2 (vec2-x cur) y) '$)))
            (for ([x (inclusive-range (vec2-x prev)
                                      (vec2-x cur)
                                      (dir (vec2-x cur) (vec2-x prev)))])
              (set! env (hash-set env (vec2 x (vec2-y cur)) '$)))))))

  env)

(define (simulate-until-done env threshold (n 0))
  (let ([simulation (simulate env (vec2 500 -1) threshold)])
    (match simulation
      ['done (values env n)]
      [new-env (simulate-until-done new-env threshold (add1 n))])))

(define (solution input)
  (define lines (parse input))
  (define threshold (+ 2 (apply max (map vec2-y (flatten lines)))))
  (define env (build-env lines))

  (let-values ([(_ number) (simulate-until-done env threshold)])
    (add1 number)))

(check-equal? (solution "test/1") 93)

(solution "input")
