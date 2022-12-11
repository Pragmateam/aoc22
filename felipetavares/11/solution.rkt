#lang racket

(require rackunit
         data/queue)

(struct monkey (items operation test true-case false-case inspections) #:mutable)

(define (set-inspections! monkeys m new-inspections)
  (set-monkey-inspections! (vector-ref monkeys m) new-inspections))

(define (throw! monkeys dst new-worry)
  (define dst-monkey (vector-ref monkeys dst))
  (enqueue! (monkey-items dst-monkey) new-worry))

(define (round! monkeys)
  (for ([m (range (vector-length monkeys))])
    (match (vector-ref monkeys m)
      [(monkey items op test dst-true dst-false _)
       (for ([_ (range (queue-length items))])
         (set-inspections! monkeys m (+ (monkey-inspections (vector-ref monkeys m)) 1))
         (let* ([worry (dequeue! items)]
                [new-worry (quotient (op worry) 3)])
           (if (= (remainder new-worry test) 0)
               (throw! monkeys dst-true new-worry)
               (throw! monkeys dst-false new-worry))))])))

(define (solution monkeys)
  (for ([m (range (vector-length monkeys))])
    (let ([items (monkey-items (vector-ref monkeys m))]
          [queue (make-queue)])
      (set-monkey-items! (vector-ref monkeys m) queue)
      (for ([item items])
        (enqueue! queue item))))

  (for ([_ (range 20)])
    (round! monkeys))

  (apply *
         (vector->list
          (vector-take
           (vector-sort
            (vector-map monkey-inspections monkeys)
            >)
           2))))

(define test-solution (solution (vector (monkey '(79 98) (λ (x) (* x 19)) 23 2 3 0)
                                        (monkey '(54 65 75 74) (λ (x) (+ x 6)) 19 2 0 0)
                                        (monkey '(79 60 97) (λ (x) (* x x)) 13 1 3 0)
                                        (monkey '(74) (λ (x) (+ x 3)) 17 0 1 0))))

(check-equal? test-solution 10605)

(solution (vector
           (monkey '(54 89 94) (λ (x) (* x 7)) 17 5 3 0)
           (monkey '(66 71) (λ (x) (+ x 4)) 3 0 3 0)
           (monkey '(76 55 80 55 55 96 78) (λ (x) (+ x 2)) 5 7 4 0)
           (monkey '(93 69 76 66 89 54 59 94) (λ (x) (+ x 7)) 7 5 2 0)
           (monkey '(80 54 58 75 99) (λ (x) (* x 17)) 11 1 6 0)
           (monkey '(69 70 85 83) (λ (x) (+ x 8)) 19 2 7 0)
           (monkey '(89) (λ (x) (+ x 6)) 2 0 1 0)
           (monkey '(62 80 58 57 93 56) (λ (x) (* x x)) 13 6 4 0)))
