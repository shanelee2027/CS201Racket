#lang racket
(define a (list 'A))
(cons 4 a)
(define (hex-digit-to-int n)
    (if (number? n)
        n
        (cond
          [(equal? n 'A)
           10]
          [(equal? n 'B)
           11]
          [(equal? n 'C)
           12]
          [(equal? n 'D)
           13]
          [(equal? n 'E)
           14]
          [(equal? n 'F)
           15])))