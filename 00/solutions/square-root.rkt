#lang racket

(provide my-sqrt)

(define (round-off z n)
  (let ((power (expt 10 n)))
    (/ (round (* power z)) power)))

(define (my-sqrt x)
  (define (call-me last-res goal-res)
    (if (= (round-off last-res 4) (round-off goal-res 4))
        (round-off last-res 4)
        (call-me (exact->inexact
                  (/ (+ last-res (exact->inexact (/ x last-res))) 2))
                 goal-res)))
  (call-me 10 (sqrt x)))

