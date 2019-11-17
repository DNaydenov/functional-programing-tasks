#lang racket

(provide from-numeral
         to-numeral
         plus
         mult
         pred)

(define zero (lambda (f v) v))

(define (succ n)
  (lambda (f v)
    (f (n f v))))


(define (from-numeral n)
  (n (lambda (i) (+ i 1)) 0))

(define (to-numeral n)
    (if (zero? n) zero
        (succ (to-numeral (- n 1)))))

(define (plus n m)
   (lambda (f v)
        (m f (n f v))))

(define (mult n m )
  (lambda (f v)
       (n
        (lambda (val) (m f val))
        v)))

(define (pred n) void)
