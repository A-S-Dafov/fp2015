#lang racket
(define (make-tree node left right)
  (list node left right))
(define (root tree)
  (car tree))

(define (make-leaf node)
  (make-tree node '() '()))

(define (empty-tree? tree)
  (null? tree))

(define (left tree)
  (first (rest tree)))

(define (is-leaf? tree)
  (if(and (equal? '() (car (rest tree))) (equal? '() (car(cdr (cdr tree))))) #t #f))

(define (right tree)
  (first (rest (rest tree))))

(define t
  (make-tree 1
    (make-tree 2
      (make-leaf 5)
      (make-leaf 6))
    (make-leaf 3)))

(define binTree
  (make-tree 10
    (make-tree 5
      (make-leaf 3)
      (make-leaf 6))
    (make-leaf 15)))

(define g
  (make-tree 1
     (make-tree 1
         (make-tree 1
             (make-leaf 1)
             (make-leaf 2))
         (make-leaf 2))
     (make-tree 2
         (make-tree 3
             (make-tree 5
                 (make-leaf 3)
                 (make-leaf 4))
             (make-leaf 6))
         (make-tree 4
             (make-leaf 7)
             (make-leaf 8)))))

(define h(make-leaf 1))

(define (height tree)
  (cond
    [(empty? tree) 0]
    [else (+ 1 (max (height (left tree)) (height (right tree))))]))
 
(define (tree-level level tree)
    (cond
      [(empty-tree? tree) '()]
      [(= level 1) (list (root tree))]
      [else (append (tree-level (- level 1) (left tree)) (tree-level (- level 1) (right tree)))]
      ))

(define (tree-map f tree)
  (cond
    [(empty-tree? tree) tree]
    [else (make-tree (f (root tree)) (tree-map f (left tree)) (tree-map f (right tree)))]))

(define (all-levels tree)
  (map (lambda (level) (tree-level level tree))
       (range 1 (+ 1 (height tree)))))
 
(define (bst-insert x tree)
  (cond
    [(empty-tree? tree) (make-leaf x)]
    [(= x (root tree)) tree]
    [(< x (root tree)) (make-tree (root tree) (bst-insert x (left tree)) (right tree))]
    [(> x (root tree)) (make-tree (root tree) (left tree) (bst-insert x (right tree)))]))

(define (bst-element? x tree)
  (cond
    [(empty-tree? tree) #f]
    [(= x (root tree)) #t]
    [(< x (root tree)) (bst-element? x (left tree))]
    [(> x (root tree)) (bst-element? x (right tree))]))
    
    (define (bst? tree)
  (cond
    [(is-leaf? tree) #t]
    [(or (< (root tree) (root (left tree))) (> (root tree) (root (right tree)))) #f]
    [else (and (bst? (left tree)) (bst? (right tree)))]))
