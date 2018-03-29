#lang planet neil/sicp

(define (bigger-sum a b c)
  (cond ((and (> a b) (> b c)) (+ a b))
        ((and (> a c) (> c b)) (+ a c))
        ((and (> b a) (> a c)) (+ b a))
        ((and (> b c) (> c a)) (+ b c))
        ((and (> c a) (> a b)) (+ c a))
        ((and (> c b) (> b a)) (+ c b))))

(define (bigger-sum2 a b c)
  (cond ((and (> a b) (> c b)) (+ a c))
        ((and (> a c) (> b c)) (+ a b))
        ((and (> b a) (> c a)) (+ b c))))