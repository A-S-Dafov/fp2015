#lang racket
(define (cube x)
  (* x x x))
(define (cube-sums? n)
  (define (helper a b)
    (cond[(= n (+ (cube a) (cube b))) #t]
         [(and (= a n) (= b n)) #f]
         [(or (> (cube a) n) (> (cube b) n)) (if(and (<= a n) (< b n)) (helper a (+ 1 b)) (helper (+ 1 a) (+ 1 a)))]
         [(and (<= a n) (< b n)) (helper a (+ 1 b))]
         
         [(and (< a n) (= b n)) (helper (+ 1 a) (+ 1 a))]))
  (helper 1 1))
         
  (define (count-cube-sums from to)
    (define (helper a br)
      (cond[(= a to) br]
           [(cube-sums? a) (helper (+ 1 a) (+ 1 br))]
           [else (helper (+ 1 a) br)]))
    (helper from 0))
      
      
