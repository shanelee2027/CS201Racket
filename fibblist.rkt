#lang racket

(define (test num-list)
  (set! num-list (append num-list (list (list-ref num-list (- (length num-list) 1)))))
  num-list)
  

(define (fibb-list-recur num-list n)
  (cond
    [(equal? n 0)
      num-list]
    [else
      (set! num-list (append num-list (list (+ (list-ref num-list (- (length num-list) 1)) (list-ref num-list (- (length num-list) 2))))))
      (set! num-list (fibb-list-recur num-list (- n 1)))
      num-list
      ]))


; (fibb-list 10) results in '(1 1 2 3 5 8 13 21 34 55)
(define (fibb-list n)
  (cond
    [(equal? n 1)
     (list 1)]
    [(equal? n 2)
     (list 1 1)]
    [else
     (fibb-list-recur (list 1 1) (- n 2))]))
       

  