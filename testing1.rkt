#lang racket
(define a (list 1 2 3 4))

(define (pie)
  3)

(define pie2 (pie))

(define (power-set lst)
  (define smaller-power-set (power-set (remove (car lst) lst)))
  4)

