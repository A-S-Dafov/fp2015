#lang racket
(provide to-binary-string)
(provide from-binary-string)
(provide string-reverse)

(define (string-reverse str)
  (define (helper result count)
    (if(= count -1) result
       (helper (string-append result (~a(string-ref str count))) (- count 1))))
  (helper (~a( string-ref str (- (string-length str) 1))) (- (string-length str) 2)))


(define (to-binary-string n)
  (define (helper result buff)
    (if(= buff 0) (string-reverse result)
       (helper (string-append result (~a (remainder buff 2))) (quotient buff 2))))
  (helper (~a (remainder n 2)) (quotient n 2)))

(define (from-binary-string binary-str)
  (define (helper result count buff expo)
    (if(= buff 0) result
       (helper (+  result (* (remainder buff 10) expo)) (+ 1 count) (quotient buff 10) (* expo 2))))
  (helper 0 0 (string->number binary-str) 1))
    
