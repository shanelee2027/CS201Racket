#lang racket
(define (phonebook name)
  (let
      [(contacts '())]
    (lambda (cmd . args)
      (case cmd
        [(add!)
         (set! contacts (cons (first args) contacts))]
        [(search!)
         (ormap (lambda (x) (equal? x (first args)))
                contacts)]
        [(display!)
         contacts]))))
