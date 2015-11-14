#lang racket
(define (sum numbers)
  (if(null? numbers) 0
       (+ (car numbers) (sum (rest numbers)))))

(define (member? x items)
  (cond[(null? items) #f]
       [(equal? x (car items)) #t]
       [else (member? x (cdr items))]))

(define (length2 items)
  (define (helper count items)
    (if(null? items) count
       (helper (+ 1 count) (cdr items))))
  (helper 0 items))

(define (list-ref2 items n)
  (if(= n 0) (car items)
     (list-ref2 (cdr items) (- n 1))))

(define (range2 a b)
  (if(= a b) '()
     (cons (list a) (range2 (+ a 1) b))))

(define (build-list2 n f)
  (if(null? n) '()
     (append (list (f (car n))) (build-list2 (cdr n) f))))

(define (append2 l1 l2)
  (define (helper l1 l2)
    (cond[(null? l2) '()]
         [(null? l1) (cons (car l2) (helper l1 (cdr l2)))]
         [else (cons (car l1) (helper (cdr l1) l2))]))
 (cond[(and (null? l1) (null? l2)) '()]
       [(null? l1) l2]
       [(null? l2) l1]
       [else (helper l1 l2)]))

(define (reverse2 items)
  (define (helper xs items)
    (if(null? items) xs
       (helper (cons (car items) xs) (cdr items))))
  (helper '() items))

(define (take2 n items)
  (cond
    [(or(> n (length2 items)) (= n 0)) '()]
    [(> n 0) (cons (car items) (take2 (- n 1) (cdr items)))]
    ))
(define (drop2 n items)
  (cond
    [(> n (length2 items)) '()]
    [(> n 0) (drop2 (- n 1) (cdr items))]
    [else items]))
(define (take-while p items)
  (cond
    [(null? items) '()]
    [(p (car items)) (cons (car items) (take-while p (cdr items)))]
    [else (take-while p '())]))
(define (drop-while p items)
  (cond
    [(null? items) '()]
    [(p (car items)) (drop-while p (cdr items))]
    [else items]))
(define (number->list n)
  (define (helper n xs)
    (cond
      [(= n 0) xs]
      [else (helper (quotient n 10) (cons (remainder n 10) xs))]))
  (if ( = n 0) 0
      (helper n '())))
(define (list->number ns)
  (define (helper n ns)
    (cond
      [(null? ns) n]
      [else (helper (+ (* n 10) (car ns)) (cdr ns))]))
  (helper 0 ns))
