#lang sicp
;(#%require "chap4-common.rkt")


(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-application lambda parameters)
  (cons lambda parameters))

"这题本来想这么做:把原本匿名的lambda 通过一个外部lambda 的参数绑定
((lambda (f)
  (f 0 1 5))
 (lambda (a b count)
   (if (= count 0)
       b
       (f (+ a b) a (- count 1)))))
但是发现不行，因为在外部lambda 求值f 的时候，f 的环境是扩展自global 环境，而不是外部lambda 环境，所以找不到他自己的（f） 的绑定
"
(define (named-let name vars bindings body)
  (make-application
   (make-lambda (list name)
                (make-application name bindings))
   (make-lambda vars
                body)))
;;这样是不行的
"
(display
 (named-let 'fib-iter '(a b count) '(0 1 5) '(if (= count 0)
                                                b
                                                (fib-iter (+ a b) a (- count 1)))))

"

(define (make-definition var value)
  (list 'define var value))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;;(let <name> <bindings> <body>)
(define (named-let? exp)
  (and (tagged-list? exp 'let)
       (symbol? (cadr exp))))

(define (named-let-name exp)
  (cadr exp))
(define (named-let-bindings exp)
  (caddr exp))
(define (named-let-body exp)
  (cadddr exp))



(define (expand-var items)
  (if (null? items)
      '()
      (cons (caar items)
            (expand-var (cdr items)))))

(define (expand-exp items)
  (if (null? items)
      '()
      (cons (cadar items)
            (expand-exp (cdr items)))))

(define (let->combination exp)
  (if (named-let? exp)
      (list 'begin
            (make-definition (named-let-name exp)
                             (make-lambda (expand-var (named-let-bindings exp))
                                          (list (named-let-body exp))));;这里要不要list？ 如果有list 则display 出来的就是scheme 的标准语法
            (make-application (named-let-name exp)
                              (expand-exp (named-let-bindings exp))))
      (make-application
       (make-lambda (expand-var (let-items exp))
                    (let-body exp))
       (expand-exp (let-items exp)))))

(define (let-items exp) '());不是重点
(define (let-body exp) '());不是重点

;;测试
(define test-case '(let fib-iter ((a 0) (b 1) (count 5)) (if (= count 0)
                                                             b
                                                             (fib-iter (+ a b) a (- count 1)))))

(named-let? test-case)
(named-let-name test-case)
(display (named-let-bindings test-case))
(newline)
(display (named-let-body test-case))
(newline)
(display (let->combination test-case))



(begin
  (define fib-iter
    (lambda (a b count)
      (if (= count 0)
          b
          (fib-iter (+ a b) a (- count 1)))))
  (fib-iter 0 1 5))