#lang racket
(define (prime? n)
  (define (help current)
    (cond [(> current (sqrt n)) #t]
          [(= (remainder n current) 0) #f]
          [else (help (+ 1 current))]
          )
    )
  (if(= n 1) #f (help 2))
  )
   ;tazi zada4a ya reshavahme v chas pri Andrei , moje da sa6tata kato na drugi hora;
