#lang racket

(provide all?
         any?
         concat
         rows
         cols
         matrix-ref
         set
         place
         diag
         diags
         map-matrix
         filter-matrix
         zip-with
         zip-matrix)
; the provide "exports" these functions

; 00. re
(define (all? p? xs)
 (foldr (lambda (xs rec)
          (and (p? xs)
               rec))
        #t xs))


; 01. re
(define (any? p? xs)
  (foldr (lambda (xs rec)
          (or (p? xs)
               rec))
        #f xs))

  

; 02. re
(define (concat xss)
  (foldr (lambda (xs rec)
           (append xs rec))
         `() xss ))

; 03. re
(define (rows xss) xss)

; 04. ? to do 
(define (cols xss)
  (if (null? (car xss)) `()
      (cons (map car xss)
            (cols (map cdr xss)))))

; 05.
(define (matrix-ref xss i j)
  (cond ((null? xss) -1)
        ((zero? i) (list-ref (car xss) j))
        (else (matrix-ref (cdr xss) (- i 1) j))))

; 06.
(define (set xs i x)
  (cond ((null? xs) `())
        ((zero? i) (cons x (cdr xs)))
        (else (cons (car xs) (set (cdr xs) (- i 1) x)))))

; 07.
(define (place xss i j x)
  (cond ((null? xss) `())
        ((zero? i) (cons (set (car xss) j x)
                         (cdr xss)))
        (else (cons (car xss)
                    (place (cdr xss) (- i 1) j x)))))

; 08.
(define (diag xss)
  (if (null? xss) `()
      (cons (car (car xss))
            (diag (map cdr (cdr xss))))))

; 09.
(define (diags xss)
  (define (reverse-matrix xss)
    (if (null? xss) `()
        (append (reverse-matrix (cdr xss)) (list (car xss)))))
  (list (diag xss) (reverse (diag (reverse-matrix xss)))))

; 10.
(define (map-matrix f xss)
  (map (lambda (xs) (map f xs)) xss))

;(define (1+ x) (+ 1 x))
(define (id x) x)
(define (const x) (lambda (y) x))
(map-matrix id         '((1337)))  ;-- '((1337))
;(map-matrix 1+         '((1 0)
 ;                       (0 1)))   ;-- '((2 1)
                                   ;     (1 2))
(map-matrix (const 69) '((1 2 3)
                         (4 5 6)
                         (7 8 9))) ;-- '((69 69 69)
                                   ;     (69 69 69)
                                   ;     (69 69 69))


; 11.
(define (filter-matrix p? xss)
  (if (null? xss) `()
      (cons (filter p? (car xss))
            (filter-matrix p? (cdr xss)))))

; 12.
(define (zip-with f xs ys)
  (map f
       (take xs (min (length xs)
                     (length ys)))
       (take ys (min (length xs)
                     (length ys)))))

; 13.
(define (zip-matrix xss yss)
  (if (or (null? xss)
           (null? yss))
      `()
      (cons (zip-with cons (car xss) (car yss))
            (zip-matrix (cdr xss) (cdr yss)))))
