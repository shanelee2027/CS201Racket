#lang racket
; 이세인 
(define (fibb n)
  (cond
    [(equal? n 1)
     1]
    [(equal? n 2)
     1]
    [else
     (+ (fibb (- n 1)) (fibb (- n 2)))]))