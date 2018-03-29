#lang planet neil/sicp


(define (SUM-INT A B)
  (if (> A B)
      0
  (+ A
     (SUM-INT (+ A 1) B))))

(SUM-INT 1 5)

(define (SUM-INT-QUICK A B)
  (if (> A B)
      0
  (/ (* (+ 1 (- B A)) (+ A B)) 2)))

(SUM-INT-QUICK 1 5)

(define (SUM term a next b)
  (if (> a b)
      0
      (+ (term a)
         (SUM term (next a) next b))))

(define (SUM-INT-SIMPLE a b)
  (define (ident x) x)
  (define (inc x) (+ x 1))
  (SUM ident a inc b))

(SUM-INT-SIMPLE 1 5)

(define (SUM-ITER term a next b)
  (define (iter j result)
    (if (> j b)
    result
    (iter (next j)
          (+ (term j) result))))
  (iter a 0))

(define (SUM-INT-ITER a b)
  (define (ident x) x)
  (define (inc x) (+ x 1))
  (SUM-ITER ident a inc b))

(SUM-INT-ITER 1 5)

(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (good-enough? new old)
    (< (abs (- new old)) tolerance))
  
  (define (iter new old)
    (if (good-enough? new old)
        new
        (iter (f new) new)))
  
  (iter (f start) start))

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (SQRT x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
(SQRT 4)

(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))
(define (new-fixed-point f start)
  (define tolerance 0.00001)
  (define (good-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (iter new old)
    (display old)
    (display ",")
    (display new)
    (newline)
    (if (good-enough? new old)
        new
        (iter (f new) new)))
  (iter (f start) start))

(define xtox
  (new-fixed-point (lambda (x) (/ (log 1000) (log x)))
                   2.0))
xtox

(define xtoxa
  (new-fixed-point (average-damp (lambda (x) (/ (log 1000) (log x))))
                   2.0))
xtoxa

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter (- k 1) (/ (n k) (d k))))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           15)

(define (cont-frac-re n d k)
  (define (iter i)
    (if (= i k)
        (/ (n k) (d k))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(cont-frac-re (lambda (i) 1.0)
              (lambda (i) 1.0)
              15)

(define (mod a b)
  (define div (floor (/ a b)))
  (if (< a b)
      a
      (- a (* div b))))

(cont-frac (lambda (i) 1.0)
           (lambda (i) (if (= (mod i 3) 2)
                           (- i (/ i 3))
                           1))
           25)

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (- (* x x))))
             (lambda (i) (- (* i 2) 1))
             k))