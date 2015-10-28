#lang racket
(require "fence.rkt" )
(require "binary.rkt")

(define (nth-beast-number n)
  (if(= n 0) "nevaliden vhod"
     (string->number (string-repeat "666" n)))
)

(define (odd-1s x)
  (define (helper num curr count)
    (
     cond[ (= num 0)  count]
         [(= curr 1) (helper (quotient num 10) (remainder num 10) (+ 1 count))]
         [else (helper (quotient num 10) (remainder num 10)  count)]))
  (cond[(= x 1) 1]
  [(< x 10) 0]
  [else (helper x (remainder x 10) 0)])
     )
    
(define (next-hack n)
  ( cond[(and (equal?
               (to-binary-string(+ 1 n)) (string-reverse (to-binary-string(+ 1 n))))
              (equal?
               #t (odd? (odd-1s(string->number(to-binary-string (+ 1 n))))))) (+ 1 n)]
        [else (next-hack (+ 1 n))]))

(define (p-score n)
  (if(equal? (number->string n) (string-reverse(number->string n))) 1
     (+ 1 (p-score (+ n (string->number(string-reverse(number->string n))))))))
