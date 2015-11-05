#lang racket
(require "prime.rkt")

(define (add1 x) (+ 1 x))

(define (square x) (* x x))

(define (f p g h)
  (lambda(x)
    (if(and (p(g x)) (p(h x))) #t
       #f)))

(define (fst pair)
  (car pair))

(define (smaller x y)
  (if(< x y) x y))
         
(define (snd pair)
  (cdr pair))

(define (add-frac frac1 frac2)
  (cond[(equal? 0 (* (snd frac1) (snd frac2))) "losho"]
       [(equal? 0 (+(* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1)))) (cons 0 0)]
  [else (cons (+(* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1)))  (* (snd frac1) (snd frac2)))]
  ))

(define (substract-frac frac1 frac2)
  (cond[(equal? 0 (* (snd frac1) (snd frac2))) "losho"]
       [(equal? 0 (-(* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1)))) (cons 0 0)]
  [else (cons (-(* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1)))  (* (snd frac1) (snd frac2)))]))

(define (mult-frac frac1 frac2)
  (cond[(equal? 0 (*(snd frac1) (snd frac2))) "losho"]  
       [(equal? 0 (* (fst frac1) (fst frac2))) (cons 0 0)]
      [else (cons (* (fst frac1) (fst frac2)) (*(snd frac1) (snd frac2)))]))

(define (simplify-frac frac)
  (define (helper n)
    (if(and (= 0 (remainder(fst frac) n)) (= 0 (remainder (snd frac) n))) (cons (/ (fst frac) n) (/ (snd frac) n))
       (helper (- n 1))))
  (helper (smaller (fst frac) (snd frac))))
