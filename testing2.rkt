#lang racket
(struct point (x y))
(define (point-add point1 point2)
  (cond
    [(and (point? point1) (point? point2))
     (point (+ (point-x point1) (point-x point2)) (+ (point-y point1) (point-y point2)))]
    [else
     null]))

(define (add-one lst)
  (cond
    [(empty? lst)
     '()]
    [(number? (first lst))
     (cons (+ 1 (first lst)) (add-one (rest lst)))]
    [else
     (cons (add-one (first lst)) (add-one (rest lst)))]))

(define (test)
  (let*
      [(var1 2)
       (var2 (+ 1 var1))]
    (+ var1 var2)))