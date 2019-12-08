#lang racket

(provide winner
         play)

(require "matrix.rkt")
; You can use your matrix functions below, thanks to the "require" invocation above.

(define (id x) x)

; winner implementation that only detects draws right now.
; Put your own implementation here!
(define (winner b)
  (define (winner-x? symbol)
     (ormap (lambda (xs) (all? (lambda (x) (eqv? x symbol)) xs))
            (append (rows b) (cols b) (diags b))))
        
  (cond ((winner-x? "X") "X")
        ((winner-x? "O") "O")
        ((andmap (lambda (xs) (andmap id xs)) b) "D")       
        (else #f)))

(define (possible-turns b)
  (define (iter i j)
    (cond ((= i (length b)) `())
          ((= j (length b)) (iter (+ i 1) 0))
          ((eqv? (matrix-ref b i j ) #f) (cons (cons i j) (iter i (+ j 1))))
          (else (iter i (+ j 1)))))
    (iter 0 0))
; "Dumb" "AI", plays the "next" free spot, going left-to-right, top-to-bottom.
; Put your own implementation here!



(define (best-outcome game p maximilize? perspective)
  (define minimax (if (eqv? maximilize? #t) max min))
  (define not-p (if (eqv? p "O") "X" "O"))
  (define winner. (winner game))
  (define possible-turns. (possible-turns game))
  
  (cond ((eqv? winner. perspective) 1)
        ((eqv? winner. "D") 0)
        ((eqv? winner. (if (eqv? perspective "O") "X" "O")) -1)
        (else (foldr  minimax (if maximilize? -200 200)
                      (map (lambda (ij)
                             (best-outcome (place game (car ij) (cdr ij) p) not-p (not maximilize?) perspective))
                           possible-turns.))))) 


 
(define (play-1 curr-board curr-sign)
  (define not-p (if (eqv? curr-sign "O") "X" "O"))
  (car (foldl (lambda (pair rec) (if (> (cdr pair) (cdr rec)) pair rec))
         (cons (cons 0 0) -2)
         (map (lambda (turn) (cons turn
                                   (best-outcome (place curr-board (car turn) (cdr turn) curr-sign) not-p #f curr-sign)))
              (possible-turns curr-board)))))

(define (play curr-board curr-sign)
  (play-1 curr-board (if curr-sign "X" "O")))