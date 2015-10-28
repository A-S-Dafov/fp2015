#lang racket
(provide
 string-repeat)

(define (string-repeat str n)
  (define (helper a count)
    (if(= count n) a
       (helper (string-append a str) (+ 1 count))))
  (helper str 1))

(define (fence n)
  (string-append "{" (string-repeat "-" (round (+ 1 (log n)))) ">" (number->string n)  "<" (string-repeat "-" (round (+ 1 (log n)))) "}"))
