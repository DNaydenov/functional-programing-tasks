#lang racket

(provide my-sqrt)

(define (my-sqrt x)
  (define (call-me last-res last-last)
    (if (<= (abs (- last-res last-last)) 0.00001)
        last-res
        (call-me (/ (+ last-res (/ x last-res)) 2.0)
                 last-res)))
  (call-me 10 0))
