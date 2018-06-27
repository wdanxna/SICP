#lang sicp
;;syntax procedure
;;仿照begin
(define (and? exp) (tagged-list? exp 'and))
(define (and-expressions exp) (cdr exp))
(define (and-first-exp exp) (cadr exp))
(define (and-rest-exp exp) (cddr exp))
(define (and-null-exp? exp) (null? (cdr exp)))

(define (or? exp) (tagged-list? exp 'or))
(define (or-expressions exp) (cdr exp))
(define (or-first-exp exp) (cadr exp))
(define (or-rest-exp exp) (cddr exp))
(define (or-null-exp? exp) (null? (cdr exp)))

;;evaluation procedure
(define (eval-and exp env)
  (define (eval-iter exps env)
    (if (null? exps)
        true
        (if (false? (eval (car exps)) env)
            false
            (if (null? (cdr exps))
                (eval (cadr exps) env);;return the last expression
                (t (cdr exps) env)))))
  (eval-iter (and-expressions epx) env))

(define (eval-or exp env)
  (define (eval-iter exps env)
    (if (null? exps)
        false
        (let ((first-eval (eval (car exps) env)))
          (cond ((true? first-eval) first-eval)
                (else (eval-iter (cdr exps) env))))))
  (eval-iter (or-expressions exp) env))

;;derived and
(define (and->if exp) (expand-and (and-expressions exp)))
(define (expand-and exps)
  (if (null? exps)
      'true
      (if (null? (cdr exps))
          (cadr exps)
          (make-if (cadr exps)
                   (expand-and (cdr exps))
                   'false))))

(define (eval-and exp env) (eval (and->if exp) env))

;;derived or
(define (or->if exp) (expand-or (or-expressions exp)))
(define (expand-or exps)
  (if (null? exps)
      'false
      (if (null? (cdr exps));last
          (cadr exps)
          (make-if (car exps)
                   (car exps)
                   (expand-or (cdr exps))))))
(define (eval-or exp env) (eval (or->if exp) env))