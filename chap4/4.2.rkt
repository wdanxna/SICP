#lang sicp

"Louis Reasoner plans to reorder the cond clauses in eval so that the clause for procedure
applications ap- pears before the clause for assignments. He argues that this will make the
interpreter more efficient: Since programs usually contain more applications than assignments,
def- initions, and so on, his modified eval will usually check fewer clauses than the original
eval before identifying the type of an expression."

"a. What is wrong with Louis’s plan? (Hint: What will Louis’s evaluator do with
the expression (define x 3)?"
;;a
;;如果对application 的分析先于definition 的话，define 会作为一个application 而不是specital form
;;因为application? 的定义是(pair? exp)

"b. Louis is upset that his plan didn’t work. He is will-
ing to go to any lengths to make his evaluator recog-
nize procedure applications before it checks for most other kinds of expressions.
Help him by changing the syntax of the evaluated language so that procedure applications
start with call. For example, instead of (factorial 3) we will now have to
write (call factorial 3) and instead of (+ 1 2) we will have to write (call + 1 2)."
;;b
;;这需要修改application 的data abstraction 的实现
(define (application? exp) (eq? (car exp) 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


;;Test
(define new-application-form (list 'call + 1 2))
(application? new-application-form) ;true
(application? (list + 1 2));false

(operator new-application-form);+
(operands new-application-form);'(1 2)
(first-operand (operands new-application-form));1
(rest-operands (operands new-application-form));'(2)