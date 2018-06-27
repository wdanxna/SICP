#lang sicp

"
let* 等价于嵌套let
"
(let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
  (* x z))

"等价于"

(let ((x 3))
  (let ((y (+ x 2)))
    (let ((z (+ x y 5)))
      (* x z))))

(define (make-let* bindings body)
  (list 'let* bindings body))

(define (let*-lets exp) (cadr exp))
(define (let*-body exp) (caddr exp))

;;测试
(define a (make-let* '((x 3) (y (+ x 2)) (z (+ x y 5))) '(* x z)))
(display (let*-lets a))
(newline)
(display (let*-body a))

(define (let*->nested-lets exp)
  (expand-lets (let*-lets exp) (let*-body exp)))

(define (expand-lets lets body)
  (if (null? lets)
      (error "let the empty")
      (if (null? (cdr lets));last
          (make-let (list (car lets))
                    body)
          (make-let (list (car lets))
                    (expand-lets (cdr lets) body)))))

(define (make-let bindings body)
  (list 'let bindings body))


;;测试
(newline)
(display (let*->nested-lets a))

"is it sufficient to add a clause to eval whose action is
(eval (let*->nested-lets exp) env)
or must we explicitly expand let* in terms of non-derived
expressions?"
;;如果eval 安装了let 这个clause 的话就可以