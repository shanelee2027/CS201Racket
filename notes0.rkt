#lang racket
(define (extract str)
  (substring str 4 7))

(define pie 3)

(define (piece str b)
  (substring str 0 b))

(define (bake flavor)
  (printf "preheating oven...\n")
  (string-append flavor " pie"))

(define (reply s)
  (if (string-prefix? s "hello ")
      "hi!"
      "huh?"))

(define (reply2 s)
  (if (and (string? s)
           (string-prefix? s "hello "))
      "hi!"
      "huh?"))

(define (reply-more s)
  (cond
    [(string-prefix? s "hello ")
     "hi!"]
    [(string-prefix? s "goodbye ")
     "bye!"]
    [(string-suffix? s "?")
     "I don't know"]
    [else "huh?"]))

(define (twice f a)
  (f (f a)))

(twice (lambda (s) (string-append s "!?")) "hello")

(let* ([x (random 4)]
      [y (+ x (random 4))])
  (+(+ y x) 1))

(printf "\n")
  
(length (list "hop" "skip" "jump"))
  

(map (lambda (s n) (substring s 0 n))
     (list "apple" "banana" "orange")
     (list 3 2 5))

(foldl (lambda (elem v)
           (+ v (* elem elem)))
         0
         '(1 2 3))

(define mylist empty)

(cons "head" mylist)
(cons "dead" mylist)