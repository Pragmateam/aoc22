#lang racket

(require racket/string)
(require racket/file)
(require rackunit)

(define (find-sizes input)
  (define path '("/"))
  (define sizes (hash))

  ; split the file in lines
  (for ([line (string-split (file->string input) "\n")])
       ; for each different kind of line
       (match (string-split line " ")
	      [`("$" "cd" "/") (set! path '("/"))] ; set path to /
	      [`("$" "cd" "..") (set! path (rest path))] ; remove the latest directory from path
	      [`("$" "cd" ,dir) (set! path (cons dir path))] ; add "dir" to path
	      [`("$" "ls") '()]
	      [`("dir" ,_name) '()]
	      [`(,size ,_name) ; if it's a file with a size
		; add the size to the current path in the dictionary, and also all the other paths above it:
		; i.e.: if the path is /a/b/c, add "size" to /a/b/c, /a/b /a and /
		(for ([i (inclusive-range 1 (length path))])
		     (set! sizes
		       (hash-update
			 sizes
			 (take-right path i)
			 (lambda (previous-size) (+ previous-size (string->number size)))
			 0)))]))

  ; return the sizes dictionary
  sizes)

(define (total-size-sum sizes)
  (apply + (filter (λ (size) (<= size 100000))
		   (hash-map sizes (λ (path size) size))))) 

(define (solution input)
  (total-size-sum (find-sizes input)))

(check-equal? (solution "test/1") 95437)

(solution "input")
