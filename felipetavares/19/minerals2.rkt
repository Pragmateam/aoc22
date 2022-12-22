#lang racket

(require racket/file
         racket/string
         racket/place
         rackunit)

(define (parse input)
  (define (parse-costs str)
    (define raw-costs (string-split str " "))

    (cons (string->symbol (second raw-costs)) (string->number (first raw-costs))))

  (define (parse-robots line)
    (define matches (regexp-match* #rx"([a-z]+[ ]robot)|([0-9]+[ ][a-z]+)" line))

    (cons (first (string-split (first matches) " "))
          (map parse-costs (rest matches))))

  (define (parse-blueprint str)
    (define lines (string-split str #rx"\\.|\\:"))
    (define robots (map parse-robots (rest lines)))
    (define robots-hash (make-hash))

    (for ([robot robots])
      (hash-set! robots-hash (string->symbol (first robot)) (rest robot)))

    robots-hash)

  (define raw-blueprints (string-split (file->string input) "\n"))

  (map parse-blueprint raw-blueprints))

(define (can-build-robot? blueprint rocks name)
  (andmap identity
          (for/list ([cost (hash-ref blueprint name)])
            (>= (hash-ref rocks (car cost) 0) (cdr cost)))))

(define (enough-of-robots? costs robots kind)
  (>= (hash-ref robots kind 0) (hash-ref costs kind 0)))

(define (update-rocks rocks robots)
  (for ([kind (hash-keys robots)])
    (set! rocks (hash-update rocks kind (lambda (n) (+ n (hash-ref robots kind))) 0)))
  rocks)

(define (pay-for-robot blueprint rocks name)
  (define rocks~ rocks)

  (for ([cost (hash-ref blueprint name)])
    (set! rocks~ (hash-update rocks~ (car cost) (lambda (value) (- value (cdr cost))))))
  rocks~)

(define (geodes t blueprint robots rocks (cache (make-hash)) (costs (void)))
  (when (void? costs)
    (set! costs (make-hash (map (lambda (kind) (cons kind (apply max
                                                                 (flatten
                                                                  (for/list ([costs (hash-values blueprint)])
                                                                    (let ([cost (assoc kind costs)])
                                                                      (if cost (cdr cost) 0)))))))
                                (hash-keys blueprint)))))

  ; (when (= t 10)
  ;   (displayln (format "Cache size: ~a" (hash-count cache))))

  (define (cantor x y)
    (exact-floor (+ y (arithmetic-shift (* (+ x y) (+ x y 1)) -1))))

  (define (h t a b)
    (cantor
     (cantor
      (cantor
       (+ (* t (hash-ref b 'obsidian 0)) (hash-ref a 'obsidian 0))
       (+ (* t (hash-ref b 'ore 0)) (hash-ref a 'ore 0)))
      (cantor
       (+ (* t (hash-ref b 'clay 0)) (hash-ref a 'clay 0))
       (+ (* t (hash-ref b 'geode 0)) (hash-ref a 'geode 0))))
     t))

  (if (hash-has-key? cache (h t rocks robots))
      (hash-ref cache (h t rocks robots))
      (if (= t 0)
          (hash-ref rocks 'geode 0)
          (let* ([rocks~ (update-rocks rocks robots)]
                 [best (apply max (cons
                                   (geodes (sub1 t) blueprint robots rocks~ cache costs)
                                   (for/list ([kind '(geode obsidian clay ore)])
                                     (if (and (can-build-robot? blueprint rocks kind)
                                              (or (equal? kind 'geode)
                                                  (not (enough-of-robots? costs robots kind)))
                                              (or (and (equal? kind 'geode) (> t 1))
                                                  (and (equal? kind 'obsidian) (> t 2))
                                                  (and (equal? kind 'ore) (> t 3))
                                                  (and (equal? kind 'clay) (> t 4)))
                                              )
                                         (geodes (sub1 t)
                                                 blueprint
                                                 (hash-update robots kind add1 0)
                                                 (pay-for-robot blueprint rocks~ kind)
                                                 cache
                                                 costs)
                                         0))))])
            (hash-set! cache (h t rocks robots) best)
            (hash-set! cache (h t rocks robots) best)
            best))))

(define (solution input)
  (define blueprints (take (parse input) 3))

  (define computers
    (for/list ([_ (in-list blueprints)])
      (place ch
             (define blueprint (place-channel-get ch))
             (place-channel-put ch (cons (car blueprint)
                                         (geodes 32 (cdr blueprint) #hash((ore . 1)) (hash)))))))

  (for ([i (range (length blueprints))])
    (place-channel-put (list-ref computers i) (cons i (list-ref blueprints i))))

  (for ([i (range (length blueprints))])
    (displayln (place-channel-get (list-ref computers i)))))

(provide main)
(define (main)
  (solution "input"))
