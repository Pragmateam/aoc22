#lang racket

(require racket/file
         racket/string
         rackunit)

(define (parse input)
  (map (λ (line) (regexp-match* #rx"([A-Z][A-Z])|[0-9]+" line))
       (string-split (file->string input) "\n")))

(define (list->graph lst)
  (define connections (hash))
  (define flows (hash))

  (for ([line lst])
    (match line
      [(cons valve (cons flow conns))
       (set! connections (hash-set connections valve conns))
       (set! flows (hash-set flows valve (string->number flow)))]))

  (values connections flows))

(define (vertices->edges vs)
  (apply append
         (for/list ([entry (hash->list vs)])
           (match entry
             [(cons src dsts)
              (for/list ([dst dsts])
                `(,src . ,dst))]))))

(define (floyd-marshall vertices)
  (define edges (vertices->edges vertices))
  (define dist (make-hash))
  (define inf 10e100)

  (for ([edge edges])
    (hash-set! dist edge 1))

  (for ([v (hash-keys vertices)])
    (hash-set! dist (cons v v) 0))

  (for ([k (hash-keys vertices)])
    (for ([i (hash-keys vertices)])
      (for ([j (hash-keys vertices)])
        (let ([tentative (+ (hash-ref dist (cons i k) inf)
                            (hash-ref dist (cons k j) inf))])
          (when (> (hash-ref dist (cons i j) inf) tentative)
            (hash-set! dist (cons i j) tentative))))))

  dist)

(define (score-table vertices weights)
  (define total-time 30)

  (for/hash ([v (hash-keys vertices)])
    (values
     v
     (for/list ([i (range total-time)])
       (* (- total-time i) (hash-ref weights v))))))

(define cache (make-hash))

(define (score scores dist vertex t (used (set)))
  (define vertices (hash-keys scores))
  (define interesting-vertices (filter (lambda (v) (> (first (hash-ref scores v)) 0)) vertices))

  (if (hash-has-key? cache (list used vertex t))
      (hash-ref cache (list used vertex t))
      (if (or (>= t 29) (set-member? used vertex))
          0
          (let* ([cell-score (list-ref (hash-ref scores vertex) (add1 t))]
                 [total-score (+ cell-score
                                 (apply max (cons 0
                                                  (map (lambda (v)
                                                         (score scores dist v (+ (if (> cell-score 0) (add1 t) t)
                                                                                 (hash-ref dist (cons vertex v)))
                                                                (set-add used vertex)))
                                                       interesting-vertices))))])
            (hash-set! cache (list used vertex t) total-score)
            total-score))))

(define (solution input)
  (define-values (vertices weights) (list->graph (parse input)))
  (define distances (floyd-marshall vertices))
  (define scores (score-table vertices weights))
  (score scores distances "AA" 0 (set)))

(check-equal? (solution "test/1") 1651)

(hash-clear! cache)

(solution "input")
