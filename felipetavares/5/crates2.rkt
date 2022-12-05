#lang racket

(require racket/file)
(require racket/string)
(require algorithms)
(require rackunit)

(define (space->empty char) (if (equal? char #\space) '() char))
(define (row->list row)
  (for/list ([i (range (/ (length row) 4))])
	    (space->empty (list-ref row (+ 1 (* i 4))))))
(define (fig->rows fig) (drop-right (map string->list (string-split fig "\n")) 1))
(define (parse-figure fig) (map flatten (apply zip (map row->list (fig->rows fig)))))

(define (parse-cmd cmd)
  (map string->number
       (drop (regexp-match #px"move (\\d+) from (\\d+) to (\\d+)" cmd) 1)))
(define (parse-cmds cmds)
  (map (lambda (line)
	 (match (parse-cmd line) [`(,n ,from ,to)
				   `(,n ,(- from 1) ,(- to 1))]))
	 cmds))
(define (delegate-parsing lst)
  (list (parse-figure (first lst))
        (parse-cmds (string-split (second lst) "\n"))))
(define (parse f) (delegate-parsing (string-split (file->string f) "\n\n")))

(define (move state from to n)
  (for/list ([i (range (length state))])
    (cond
      [(= i from) (drop (list-ref state i) n)]
      [(= i to) (append (take (list-ref state from) n) (list-ref state i))]
      [else (list-ref state i)])))

(define (execute state instructions)
  (if (empty? instructions)
      state
      (match (first instructions)
        [`(,n ,from ,to) (execute (move state from to n) (rest instructions))])))

(define (solution f)
  (let ((input (parse f)))
    (list->string (map first (execute (first input) (second input))))))

(check-equal? (solution "test/1") "MCD")

(solution "input")
