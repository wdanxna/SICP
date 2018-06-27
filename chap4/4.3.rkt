#lang sicp
;;假设我们有一个一维表存放exp-type 和对应类型eval 的映射
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((not (null? (get (exp-type exp)))) ((get (exp-type exp)) exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))))

(define (exp-type exp) (car exp))

;;self-evaluating ，variable，application 是没有类型前缀的，所以还是需要单独处理
;;假设get 不匹配时返回null