#lang sicp

(define (call-each procs)
  (if (not (null? procs))
      (begin
        ((car procs))
        (call-each (cdr procs)))
      'done))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (add-action! wire action) ((wire 'add-action!) action))

(define (after-delay delay proc)
  "")

(define or-gate-delay 0)
(define and-gate-delay 0)
(define not-gate-delay 0)

(define (exercise3.28)

  (define (logical-or a b)
    (cond ((= a 0) b)
          ((= b 0) a)
          ((= a 1) 1)
          ((= b 1) 1)
          (else (error "INVALID signal" a b))))
  
  (define (or-gate a1 a2 output)
    (define (or-action-procedure)
      (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
        (after-delay
         or-gate-delay
         (lambda () (set-signal! output new-value)))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure))

  "必要过程还没有实现所以没法测试")

(define (exercise3.29)

  ;;and-gate
  (define (and-gate a1 a2 output)
    (define (logical-and a b)
      (cond ((= a 0) 0)
            ((= b 0) 0)
            ((= a b) 1)
            (else (error "INVALID signal" a b))))
    
    (define (and-gate-procedure)
      (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
        (after-delay
         and-gate-delay
         (lambda () (set-signal! output new-value)))))
    (add-action! a1 and-gate-procedure)
    (add-action! a2 and-gate-procedure))

  ;;not-gate
  (define (not-gate a output)
    (define (logical-not x)
      (cond ((= x 0) 1)
            ((= x 1) 0)
            (else (error "INVALID signal" x))))
    (define (not-gate-procedure)
      (let ((new-value (logical-not (get-signal a))))
        (after-delay
         not-gate-delay
         (lambda () (set-signal! output new-value)))))
    (add-action! a not-gate-procedure))
  
  (define (or-gate a1 a2 output)
    (let ((not-a1 (make-wire))
          (not-a2 (make-wire))
          (s (make-wire)))
      (not-gate a1 not-a1)
      (not-gate a2 not-a2)
      (and-gate not-a1 not-a2 s)
      (not-gate s output)
      'ok))

  "用与门和非门构造或门，单纯用门去考虑怎么组合比较难想。要从逻辑方式去思考就很容易去设计了，或门就是：不同时为0。直接把这个断言翻译成逻辑符号就是!(!a & !b)
对于delay，因为头两个非门是并联，所以delay 按一个非门算，后面串联一个与门和非门则要累加上去，所以因该是2 * not-gate-delay + and-gate-delay
注意这题的or-gate 和上一题的区别，这里不在需要为参数添加action，因为这里知识单纯的用gate 来组合，而各个具体gate 里面已经对传入的wire 进行了add-action! 操作。"
 (list and-gate or-gate not-gate))


;;export gate definition done previously
(define gate-package (exercise3.29))
(define and-gate (car gate-package))
(define or-gate (cadr gate-package))
(define not-gate (caddr gate-package))

(define (exercise3.30)
  ;;half-adder
  (define (half-adder a b s c)
    (let ((d (make-wire))
          (e (make-wire)))
      (or-gate a b d)
      (and-gate a b c)
      (not-gate c e)
      (and-gate d e s)))

  ;;full-adder
  (define (full-adder a b c-in sum c-out)
    (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
      (half-adder b c-in s c1)
      (half-adder a s sum c2)
      (or-gate c1 c2 c-out)
      'ok))

  (define (ripple-carry-adder A B n c-in S C)
    (define (iter n A B c-in)
      (if (= n 1)
          (full-adder (car A) (car B) c-in (car S) C)
          (let ((c (make-wire)))
            (full-adder (car A) (car B) c-in (car S) c)
            (iter (- n 1) (cdr A) (cdr B) c))))
    (if (= (length A)
           (length B)
           (length C)
           n)
        (iter n A B c-in)
        (error "Signal length not equal" (length A) (length B) (length C) n))
    'ok)
  
  "")


(define (exercise3.31)
  "一开始也是想不通为什么add-action! 需要先把注册进去的proc 调用一次。要理解原因，需要
仔细分析wire 的实现和书里给出的sample simulation。
1. 看set-my-signal! 注意在值没有改变的时候，它不调用action
2. wire 的初始信号是 0
3. 设置input-1 的信号后执行propagate，只打印一个结果，但是input-1 其实在half-adder 里面链接
了两个gate，按理说prob 应该有两次输出。

这样就明了了，就是因为add-action! 的那一次调用，设置了wire 的初始状态！这样才能保证链接起来的device 能
正确运行。比如，初始signal 是0，那么not-gate 第一次调用则会将它的output wire 设置成1，这是这根wire 的初始状态
这保证了如果这次propagate 调用时这根wire 还是1 的话说明它没被激活，不需要再propagate 下去。")


