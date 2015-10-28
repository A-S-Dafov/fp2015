#lang racket
(define (circle? circle-x circle-y radius point-x point-y)
  (if(>= (* radius radius) (+ (*(- point-x circle-x) (- point-x circle-x)) (*(- point-y circle-y) (- point-y circle-y)))) #t #f)) 
