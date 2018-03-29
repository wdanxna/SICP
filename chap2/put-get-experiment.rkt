#lang planet neil/sicp
(define data-directed-module 
  ((lambda ()
     
     (define set '())
     
     (define (make-item op type item)
       (list op (list type item)))
     
     (define (put op type item)
       ())
     (define (get op type)
       2)
     (list put get))))

(define put (car data-directed-module))
(define get (cadr data-directed-module))