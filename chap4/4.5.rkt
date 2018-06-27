#lang sicp
;;cond can be derived from 'if specital form
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))

(define (test-recipient? clause)
  (eq? (cadr clause) '=>))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF" clauses))
            (if (test-recipient? first)
                (make-if (test first);;test 会被eval 两次
                         (list (recipient first) (test first));;这里是list 而不是cons 因为application 里面参数是一个list，所以为要是'()
                         (expend-clauses test))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expend-clauses rest)))))))

