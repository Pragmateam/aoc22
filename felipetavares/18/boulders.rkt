#lang racket

; My fastest solution yet!! 9m56! (spent a long time figuring out the neighbours fn)

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

(define (occluded-area cubes)
  (define occluded 0)
  (define cubeset (list->set cubes))

  (for ([a cubes])
    (for ([b (neighbours a)])
      (when (set-member? cubeset b)
          (set! occluded (add1 occluded)))))

  occluded)

(define (surface-area cubes)
  (- (* (length cubes) 6)
     (occluded-area cubes)))


(define (solution input)
  (surface-area (parse input)))

(check-equal? (solution "test/1") 64)

(solution "input")
