"为了不对底层解释器的解释顺序有任何依赖，需要显示的对表达式求值。"
"可以分别假设底层解释器是从右到左和从左到右"


;;left to right
"这里保证顺序的关键是：1.过程里只有一个表达式let。 2.显示求值了左边的表达式。从右到左同理"
(define (list-of-values exps env)
  (let ((left (eval (first-operand exps) env)))
    (cons left
          (list-of-values (rest-operands exps) env))))

;;right to left
(define (list-of-values exps env)
  (let ((right (list-of-values (rest-operands exps) env)))
    (cons (eval (first-operand exps) env)
          right)))