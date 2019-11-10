#lang racket

(provide my-sqrt)

(define (round-off z n)
  (let ((power (expt 10 n)))
    (/ (round (* power z)) power)))

(define (my-sqrt x)
  (define (call-me last-res last-last)
    (if (< (- last-res last-last) 0.0001)
        (round-off last-res 4)
        (call-me (exact->inexact
                  (/ (+ last-res (exact->inexact (/ x last-res))) 2))
                 last-res)))
  (call-me 10 0))

