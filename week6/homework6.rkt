#lang racket
(define (take-while1 x items)
  (cond
    [(null? items) '()]
    [(equal? x (car items)) (cons (car items) (take-while1 x (cdr items)))]
    [else (take-while1 x '())]))
(define (drop-while1 x items)
  (cond
    [(null? items) '()]
    [(equal? x (car items)) (drop-while1 x (cdr items))]
    [else items]))

(define (group xs)
 (cond
   [(null? xs) '()]
   [(cons (take-while1 (car xs) xs) (group (drop-while1 (car xs) xs)))]))
     
     
  (define (print-n-times n letter)
    (if(= n 0) ""
       (string-append letter (print-n-times (- n 1) letter))))

(define (run-length-encode xs)
  (define (helper i n curr result xs)
    (cond
      [(= i (string-length xs)) (if(> n 1) (string-append  result
                                 (number->string n)
                                 (make-string  1 curr)
                                  )
                                (string-append  result
                                 (make-string  1 curr)
                                  ))]
      [(equal? curr
                (string-ref xs i))
        (helper (+ i 1)
                (+ n 1)
                curr
               result
               xs)]
      [(or (equal? n 0)(equal? n 1)) (helper (+ i 1)
                            1
                            (string-ref xs i)
                            (string-append
                             result
                             (make-string  1 curr)
                             
                             )
                            xs)]
      [else (helper (+ i 1)
                    1
                   (string-ref xs i)
                    (string-append result
                                 (number->string n)
                                 (make-string  1 curr)
                                  )
                    xs)]))
  
  (helper 0 0 (string-ref xs 0) "" xs))

(define (run-length-decode lst)
  (define (helper res numb lst prev)
    (cond
      [(equal? lst "") res]
      [(number? (string->number (substring lst 0 1))) (helper res (+ (string->number(substring lst 0 1)) (* 10 numb)) (substring lst 1) (substring lst 0 1))]
      [(not (number? (string->number prev))) (helper res 1 lst "1")]
      [else (helper (string-append res (print-n-times numb (substring lst 0 1))) 0 (substring lst 1) (substring lst 0 1) )]))
  (helper "" 0 lst "a"))



                            
                
                
                
      
