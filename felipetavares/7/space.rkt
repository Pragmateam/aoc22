#lang racket

(require racket/file)
(require rackunit)

(define-namespace-anchor dsl)

(struct state (tree path))

(define (cd directory st)
  (match directory
    [".." (struct-copy state st (path (rest (state-path st))))]
    ["/" (struct-copy state st (path '("/")))]
    [dir (struct-copy state st (path (cons dir (state-path st))))]))
(define (ls output st)
  (if (empty? output)
      st
      (let ([path (state-path st)]
            [tree (state-tree st)])
        (ls (rest output)
            (state
             (if (hash-has-key? tree path)
                 (hash-set tree path (cons (first output) (hash-ref tree path)))
                 (hash-set tree path (list (first output))))
             path)))))

(define (execute cmd st)
  (let ([expr (append (list (string->symbol (first cmd))) (rest cmd) `(,st))])
    (eval expr (namespace-anchor->namespace dsl))))

(define (filesystem-tree session (st (state (hash) '("/"))))
  (if (empty? session)
      (state-tree st)
      (filesystem-tree (rest session) (execute (first session) st))))

(define (total-size fs (path '(/)))
  (apply + (map (λ (item)
                  (match item
                    [`("dir" ,d) (total-size fs (cons d path))]
                    [`(,size ,_) (string->number size)]))
                (hash-ref fs path))))

(define (total-size-sum fs)
  (apply + (hash-map fs (λ (path _)
                          (let ([size (total-size fs path)])
                            (if (<= size 100000)
                                size
                                0))))))
(define (parse-cmd cmd)
  (let ([parsed (map (λ (arg) (string-split arg " ")) cmd)])
    (cond
      [(member "cd" (first parsed)) (flatten parsed)]
      [(member "ls" (first parsed)) (cons (first (first parsed)) (list `(quote ,(rest parsed))))]
      [else parsed])))

(define (parse str)
  (map parse-cmd
       (map (λ (cmd) (string-split cmd "\n"))
            (map string-trim (string-split str "$")))))

(define (solution f)
  (total-size-sum (filesystem-tree (parse (file->string f)))))

(check-equal? (solution "test/1") 95437)

(solution "input")
