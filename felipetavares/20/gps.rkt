#lang racket

(require racket/file
         racket/string)

(define (parse input)
  (map string->number (string-split (file->string input) "\n")))

(struct tree-leaf (parent value) #:transparent #:mutable)
(struct tree-node (parent n left right) #:transparent #:mutable)

(define (tree-n node)
  (match node
    [(tree-node _ n _ _) n]
    [(tree-leaf _ _) 1]
    ))

(define (tree-from-list lst parent)
  (if (= (length lst) 1)
      (tree-leaf parent (first lst))
      (let* ([node (tree-node parent 0 (void) (void))]
             [l (quotient (length lst) 2)]
             [r (- (length lst) l)]
             [left-tree (tree-from-list (take lst l) node)]
             [right-tree (tree-from-list (take-right lst r) node)])

        (set-tree-node-n! node (+ (tree-n left-tree) (tree-n right-tree)))
        (set-tree-node-right! node right-tree)
        (set-tree-node-left! node left-tree)

        node)))

(define (tree-from-leaves lst parent)
  (if (= (length lst) 1)
      (let ([leaf (first lst)])
        (set-tree-leaf-parent! leaf parent)
        leaf)
      (let* ([node (tree-node parent 0 (void) (void))]
             [l (quotient (length lst) 2)]
             [r (- (length lst) l)]
             [left-tree (tree-from-leaves (take lst l) node)]
             [right-tree (tree-from-leaves (take-right lst r) node)])

        (set-tree-node-n! node (+ (tree-n left-tree) (tree-n right-tree)))
        (set-tree-node-right! node right-tree)
        (set-tree-node-left! node left-tree)

        node)))

(define (tree-leaf-grandparent leaf)
  (let ([grandpa (tree-node-parent (tree-leaf-parent leaf))])
    grandpa))

(define (tree-leaves tree)
  (match tree
    [(tree-node _ _  l r)
     (append (tree-leaves l) (tree-leaves r))]
    [leaf (list leaf)]))

(define (propagate-sum! tree delta)
  (match tree
    [(? tree-node? node)
     (set-tree-node-n! node (+ delta (tree-node-n node)))
     (propagate-sum! (tree-node-parent node) delta)]
    [_ (void)]))

(define (set-tree-parent! tree parent)
  (match tree
    [(? tree-node? node) (set-tree-node-parent! node parent)]
    [(? tree-leaf? leaf) (set-tree-leaf-parent! leaf parent)]))

(define (tree-remove! leaf)
  ; Make the parent equal its other child
  (if (equal? (tree-node-left (tree-leaf-parent leaf)) leaf)
      ; I am the left child
      (begin
        (if (equal? (tree-node-left (tree-leaf-grandparent leaf)) (tree-leaf-parent leaf))
            ; Parent is left child
            (set-tree-node-left! (tree-leaf-grandparent leaf) (tree-node-right (tree-leaf-parent leaf)))
            ; Parent is right child
            (set-tree-node-right! (tree-leaf-grandparent leaf) (tree-node-right (tree-leaf-parent leaf)))
            )
        (set-tree-parent! (tree-node-right (tree-leaf-parent leaf)) (tree-leaf-grandparent leaf)))
      ; I am the right child
      (begin
        (if (equal? (tree-node-left (tree-leaf-grandparent leaf)) (tree-leaf-parent leaf))
            ; Parent is left child
            (set-tree-node-left! (tree-leaf-grandparent leaf) (tree-node-left (tree-leaf-parent leaf)))
            ; Parent is right child
            (set-tree-node-right! (tree-leaf-grandparent leaf) (tree-node-left (tree-leaf-parent leaf)))
            )
        (set-tree-parent! (tree-node-left (tree-leaf-parent leaf)) (tree-leaf-grandparent leaf)))
      )

  ; Subtract 1 from n in the grandparent and up
  (propagate-sum! (tree-leaf-grandparent leaf) -1)
  )

(define (tree-insert! tree leaf index (low-left 0))
  (match tree
    [(struct tree-node _)
     (let ([low-right (+ low-left (tree-n (tree-node-left tree)))])
       (if (< index low-right)
           ; index is on the left subtree
           (tree-insert! (tree-node-left tree) leaf index low-left)
           ; index is on the right subtree
           (tree-insert! (tree-node-right tree) leaf index low-right)
           ))]
    [(struct tree-leaf _)
     (let* ([parent (tree-leaf-parent tree)]
            [new-node (if (<= index low-left)
                          (tree-node parent 2 leaf tree)
                          (tree-node parent 2 tree leaf))])
       (if (equal? (tree-node-left parent) tree)
           ; I'm the left child
           (begin
             (set-tree-node-left! parent new-node)
             (set-tree-leaf-parent! leaf new-node)
             (set-tree-leaf-parent! tree new-node))
           ; I'm the right child
           (begin
             (set-tree-node-right! parent new-node)
             (set-tree-leaf-parent! leaf new-node)
             (set-tree-leaf-parent! tree new-node))
           )

       ; Add 1 to the parent and all above
       (propagate-sum! parent 1)
       )
     ]
    )
  )

(define (leaf-index tree)
  (match tree
    [(struct tree-node _)
     (if (void? (tree-node-parent tree))
         ; I'm the root
         (sub1 (tree-n tree))
         (if (equal? (tree-node-left (tree-node-parent tree)) tree)
             ; I'm left
             (- (leaf-index (tree-node-parent tree)) (tree-n (tree-node-right (tree-node-parent tree))))
             ; I'm right
             (leaf-index (tree-node-parent tree))
             ))]
    [(struct tree-leaf _)
     (if (equal? (tree-node-left (tree-leaf-parent tree)) tree)
         ; I'm left
         (- (leaf-index (tree-leaf-parent tree)) (tree-n (tree-node-right (tree-leaf-parent tree))))
         ; I'm right
         (leaf-index (tree-leaf-parent tree))
         )]
    [_ 0]
    )
  )

(define (has-leaf? node)
  (or (tree-leaf? (tree-node-right node))
      (tree-leaf? (tree-node-left node))))

(define (solution input)
  (define tree (tree-from-list (map (lambda (n) (* n 811589153)) (parse input)) (void)))
  (define leaves (tree-leaves tree))

  (for ([_ (range 10)])
    (displayln "----")
    (for ([i (range (length leaves))])
      (let* ([leaf (list-ref leaves i)]
             [index (leaf-index leaf)]
             [new-index index]
             [index-inc (+ new-index (tree-leaf-value leaf))]
             [index-mod (modulo index-inc (sub1 (length leaves)))])
        ;(displayln (format "Moving ~a which has index ~a" (tree-leaf-value leaf) index))
        ;(displayln (format "The new index is ~a" index-mod))
        ;(displayln (map tree-leaf-value (tree-leaves tree)))
        (tree-remove! leaf)
        (tree-insert! tree leaf index-mod 0)
        ;(displayln (map tree-leaf-value (tree-leaves tree)))
          
         (when (has-leaf? tree)
           (displayln "Rebalancing...")
           (set! tree (tree-from-leaves (tree-leaves tree) (void)))
           )
          ; (displayln (map tree-leaf-value (tree-leaves tree)))
        )))

  (define lst (map tree-leaf-value (tree-leaves tree)))

  (define zero (index-of lst 0))

  (define a (list-ref lst (modulo (+ zero 1000) (length lst))))
  (define b (list-ref lst (modulo (+ zero 2000) (length lst))))
  (define c (list-ref lst (modulo (+ zero 3000) (length lst))))

  (+ a b c))

(solution "input")
