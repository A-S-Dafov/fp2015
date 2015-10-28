#lang racket
(define (series a b n)
  (define (helper x y buff)
    (cond[(= n 1) a]
         [(= n 2) b]
         [(= buff n) y]
         [else (helper y (+ x y) (+ 1 buff))]))
  (helper a b 2))

(define (fibonacci n)
  (series 1 1 n))

(define (lucas n)
  (series 2 1 n))

(define (summed-member n)
(+ (fibonacci n) (lucas n)))

(define (nth-lucas-sum n)
  (define (helper result count)
    (if(= count n) (+ result (lucas count))
       (helper (+ result (lucas count)) (+ 1 count))))
  (helper 0 1))

(define (nth-fibonacci-sum n)
  (define (helper result count)
    (if(= count n) (+ result (fibonacci count))
       (helper ( + result (fibonacci count)) (+ 1 count))))
  (helper 0 1))

(define (lucas-fibb-diff n)
  (- (lucas n) (fibonacci n)))
