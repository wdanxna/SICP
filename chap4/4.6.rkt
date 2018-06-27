#lang sicp
"
let 的语法是
(let ((<var1> <exp1>) ... (<varn> <expn>))
  <body>)

其实等价于一个lambda 的调用，lambda 的参数是var1...varn，调用将<exp1>..<expn> 作实参绑定到lambda 形参
((lambda (var1 ... varn)
  <body>) <exp1> ... <expn>)
"

(define (make-application lambda parameters)
  (list lambda parameters))

(define (let->combination exp)
  (make-application
   (make-lambda (expand-var (let-items exp))
                (let-body exp))
   (expand-exp (let-items exp))))

(define (expand-var items)
  (if (null? items)
      '()
      (cons (car items)
            (expand-var (cdr items)))))

(define (expand-exp items)
  (if (null? items)
      '()
      (cons (cadr items)
            (expand-var (cdr items)))))


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((let? exp) (eval (let->combination exp) env));;install
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (let? exp) (tagged-list? exp 'let))
  