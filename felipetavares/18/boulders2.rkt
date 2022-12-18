#lang racket

(require racket/file
         racket/string
         rackunit)

(define (parse input)
  (map
   (λ (line) (map string->number (string-split line ",")))
   (string-split (file->string input) "\n")))

(define (neighbours cube)
  (match cube
    [(list x y z)
     `((,(add1 x) ,y ,z)
       (,(sub1 x) ,y ,z)
       (,x ,(add1 y) ,z)
       (,x ,(sub1 y) ,z)
       (,x ,y ,(add1 z))
       (,x ,y ,(sub1 z)))]))

(define (surface-neighbours cubes cube)
  (define neighbourset (mutable-set))

  (for ([n (neighbours cube)])
    (when (not (set-member? cubes n))
      (set-add! neighbourset n)
      (for ([nn (neighbours n)])
        (when (not (set-member? cubes nn))
          (set-add! neighbourset nn))
        )))

  (set->list neighbourset))

(define (is-surface-cube? cubes cube)
  (and (not (set-member? cubes cube))
       (ormap (lambda (n) (set-member? cubes n)) (neighbours cube))))

(define (count-surface-faces cubes cube)
  (apply + (map (lambda (n) (if (set-member? cubes n) 1 0)) (neighbours cube))))

(define (above cube)
  (match cube
    [(list x y z) `(,x ,y ,(add1 z))]))

(define (surface cubes cube (visited (mutable-set)))
  (set-add! visited cube)

  (apply set-union
         (cons (set cube)
               (filter (compose not void?)
                       (for/list ([neighbour (surface-neighbours cubes cube)])
                         (when (and (is-surface-cube? cubes neighbour)
                                    (not (set-member? visited neighbour)))
                           (surface cubes neighbour visited)))))))

(define (areas cubes surface-cubes)
  (for/list ([s surface-cubes])
             (count-surface-faces cubes s)))

(define (surface-area cubes)
  ; Find cube right at the surface by using max along a coord
  (define surface-cube (first (sort cubes > #:key third)))

  ; Expand the surface from that point and calculate the area
  (apply + (areas (list->set cubes) (surface (list->set cubes) (above surface-cube)))))

(define (solution input)
  (surface-area (parse input)))

(check-equal? (solution "test/1") 58)

(solution "input")
