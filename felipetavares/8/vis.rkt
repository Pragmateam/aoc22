#lang racket

(require racket/file)
(require algorithms)
(require rackunit)

(define (parse input)
  (map (λ (line) (map char->integer (string->list line)))
       (string-split (file->string input) "\n")))

(define (transpose matrix)
  (for/list ([x (range (length (first matrix)))])
    (for/list ([y (range (length matrix))])
      (list-ref (list-ref matrix y) x))))

(define (cut-middle matrix)
  (for/list ([y (range 1 (- (length matrix) 1))])
    (for/list ([x (range 1 (- (length (first matrix)) 1))])
      (list-ref (list-ref matrix y) x))))

(define (visible heights w h transpose invert)
  (define th (transpose heights))

  (flatten
   (cut-middle
    (transpose
     (for/list ([y (range h)])
       (define min-height -1)
       (invert
        (for/list ([x (invert (range w))])
          (let ([height (list-ref (list-ref th y) x)])
            (if (> height min-height)
                (begin (set! min-height height) #t)
                #f)))))))))

(define (solution input)
  (let* ([heights (parse input)]
         [w (length (first heights))]
         [h (length heights)])
    (+ w w h h -4
       (apply +
              (map (λ (b) (match b [#t 1] [#f 0]))
                   (map any?
                        (zip
                         (visible heights w h identity identity)
                         (visible heights w h identity reverse)
                         (visible heights w h transpose identity)
                         (visible heights w h transpose reverse))))))))

(check-equal? (solution "test/1") 21)

(solution "input")
