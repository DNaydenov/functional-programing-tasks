#lang racket

(provide from-k-ary
         to-k-ary)

(define (from-k-ary n k) 
  (define (helper acc counter rest)
    (if (zero? rest)
        acc
        (helper (+ acc
                   (* (remainder rest 10)
                      (expt k counter))) (+ 1 counter)
                                         (quotient  rest 10))))
  (helper 0 0 n))	



(define (to-k-ary n k)
  (define (helper acc counter rest)
    (if (zero? rest)
        acc
        (helper (+ (* (remainder rest k) (expt 10 counter))
                   acc)
                (+ 1 counter)
                (quotient rest k))))
  (helper 0 0 n))

