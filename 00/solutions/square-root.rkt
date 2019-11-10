#lang racket

(provide my-sqrt)

(define (my-sqrt x)
  (define (call-me last-res last-last)
    (if (< (- last-res last-last) 0.0001)
        last-res
        (call-me (exact->inexact
                  (/ (+ last-res (exact->inexact (/ x last-res))) 2))
                 last-res)))
  (call-me 10 0))

