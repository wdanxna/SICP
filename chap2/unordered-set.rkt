#lang planet neil/sicp
;;representing sets in unordered list
(define (equal? a b)
  (cond
    ((and (null? a) (null? b)) true)
    ((and (symbol? a) (symbol? b))
     (eq? a b))
    ((and (number? a) (number? b))
     (eq? a b))
    ((and (pair? a) (pair? b))
     (and (equal? (car a) (car b))
          (equal? (cdr a) (cdr b))))
    (else false)))

(define (append a b)
  (if (null? a)
      b
      (cons (car a) (append (cdr a) b))))

(define (filter predicate sequence)
  (if (null? sequence) 
       nil
       (if (predicate (car sequence))
           (cons (car sequence) 
                 (filter predicate (cdr sequence)))
           (filter predicate (cdr sequence)))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoint-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))




;;2.59
;;iterative
(define (union-set set1 set2)
  (define (union-iter s ret)
    (if (null? s)
        ret
        (if (not (element-of-set? (car s) ret))
            (union-iter (cdr s) (cons (car s) ret))
            (union-iter (cdr s) ret))))
  (union-iter (append set1 set2) '()))

;;recursive
(define (union-set2 set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set2 (cdr set1) set2))
        (else (cons (car set1)
                    (union-set2 (cdr set1) set2)))))

;;recursive
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;;iterative
(define (intersection-set2 set1 set2)
  (define (intersection-iter s ret)
    (if (null? s)
        ret
        (if (element-of-set? (car s) set2)
            (intersection-iter (cdr s) (cons (car s) ret))
            (intersection-iter (cdr s) ret))))
  (intersection-iter set1 '()))