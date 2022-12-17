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

(define (score-table vertices weights)
  (define total-time 26)

  (for/hash ([v (hash-keys vertices)])
    (values
     v
     (for/list ([i (range total-time)])
       (* (- total-time i) (hash-ref weights v))))))

(define cache (make-hash))

(define (value-at scores lst t)
  (apply + (map (lambda (v) (list-ref (hash-ref scores v) t)) lst)))

; NOTE: This is a remarkably bad solution. It tries its best to reduce the
; search space by cutting parts of the search tree such that's just barely
; computable but, there's either:
;
; 1. A much better way of prunning the tree.
; 2. 🤔 A formulation of the k-Chinese Postman Problem which can be applied to
;   finding the optimal paths without having to search the tree.
(define (value vertices weights scores a b prev-a prev-b opened t)
  (define next-as (hash-ref vertices a))
  (define next-bs (hash-ref vertices b))

  ; (displayln (hash-count cache))
  ; (displayln (format "Called with A=~a B=~a (t=~a)" a b t))
  ; (displayln (format "The cache now has ~a entries" (hash-count cache)))
  ; (displayln (format "The currently opened valves are ~a" opened))

  (cond
    [(or (>= t 25))
     0]
    [(or (hash-has-key? cache (list (set a b) opened t)))
     (hash-ref cache (list (set a b) opened t) 0)]
    [else
     (let ([best
            (max
             ; open a, b - if not open already
             (if (and (not (set-member? opened a))
                      (not (set-member? opened b))
                      (> (hash-ref weights a) 0)
                      (> (hash-ref weights b) 0))
                 (+ (value vertices weights scores a b a b (set-add (set-add opened a) b) (add1 t))
                    (value-at scores (remove-duplicates (list a b)) (add1 t)))
                 0)
             ; open a and move b - if not open already
             (if (and (not (set-member? opened a))
                      (> (hash-ref weights a) 0))
                 (apply max
                        (map (lambda (b~) (+ (value vertices weights scores a b~ a b(set-add opened a) (add1 t))
                                             (value-at scores (list a) (add1 t)))) next-bs))
                 0)
             ; open b and move a - if not open already
             (if (and (not (set-member? opened b))
                      (> (hash-ref weights b) 0))
                 (apply max
                        (map (lambda (a~) (+ (value vertices weights scores a~ b a b(set-add opened b) (add1 t))
                                             (value-at scores (list b) (add1 t)))) next-as))
                 0)
             ; just move
             (apply max
                    (flatten (for/list ([a~ next-as])
                               (for/list ([b~ next-bs])
                                 (if (and (not (equal? a~ prev-a))
                                          (not (equal? b~ prev-b)))
                                     (value vertices weights scores a~ b~ a b opened (add1 t))
                                     0)))))
             )])

       ; cache this result
       (hash-set! cache (list (set a b) opened t) best)

       best)]))

(define (solution input)
  (define-values (vertices weights) (list->graph (parse input)))
  (define scores (score-table vertices weights))
  (value vertices weights scores "AA" "AA" "AA" "AA" (set) 0))

(check-equal? (solution "test/1") 1707)

(hash-clear! cache)

(solution "input")
