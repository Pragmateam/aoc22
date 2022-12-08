#lang racket

(require racket/file)
(require algorithms)
(require rackunit)

(define (parse input)
  (map (λ (line) (map char->integer (string->list line)))
       (string-split (file->string input) "\n")))

(define (ref heights x y) (list-ref (list-ref heights y) x))

(define (uni-scenic-score heights height x y dx dy)
  (let ([w (length (first heights))]
        [h (length heights)]
        [nx (+ dx x)]
        [ny (+ dy y)])
    (if (or (>= nx w) (>= ny h) (< ny 0) (< nx 0))
        0
        (if (>= (ref heights nx ny) height)
            1
            (+ 1 (uni-scenic-score heights height nx ny dx dy))))))

(define (scenic-score heights x y)
  (apply *
         (append
          (for/list ([dx (inclusive-range -1 1 2)])
            (uni-scenic-score heights (ref heights x y) x y dx 0))
          (for/list ([dy (inclusive-range -1 1 2)])
            (uni-scenic-score heights (ref heights x y) x y 0 dy)))))

(define (solution input)
  (let ([heights (parse input)])
    (apply max
     (flatten
      (for/list ([x (length (first heights))])
        (for/list ([y (length heights)])
          (scenic-score heights x y)))))))

(check-equal? (solution "test/1") 8)

(solution "input")
