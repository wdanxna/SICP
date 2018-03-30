#lang sicp

;;copied from previouse part
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?)
      (null? front-ptr))
    (define (insert! item)
      (let ((new-item (cons item '())))
        (cond ((empty?)
               (set! front-ptr new-item)
               (set! rear-ptr new-item))
              (else
               (set-cdr! rear-ptr new-item)
               (set! rear-ptr new-item)))))
    (define (delete!)
      (cond ((empty?)
             (error "DELETE call with empty queue"))
            (else
             (set! front-ptr (cdr front-ptr)))))
    (define (print)
      (cond ((empty?)
             (display '()))
            (else (display front-ptr))))
    "--------------------"
    (define (dispatch m)
      (cond ((eq? m 'empty?) (empty?))
            ((eq? m 'delete!) (delete!))
            ((eq? m 'insert!) insert!)
            ((eq? m 'front) (car front-ptr));注意这里要返回car 如果直接返回front-ptr 的话则是一个list
            ((eq? m 'print) (print))
            (else (error "Invalid message for queue" m))))
    dispatch))


(define (empty-queue? queue)
  (queue 'empty?))
(define (front-queue queue)
  (queue 'front))
(define (insert-queue! queue value)
  ((queue 'insert!) value))
(define (delete-queue! queue)
  (queue 'delete!))


(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda)
  (car (segments agenda)))

(define (rest-segments agenda)
  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments)) action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))

  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))


(define the-agenda (make-agenda))
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

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


(define or-gate-delay 5)
(define and-gate-delay 3)
(define not-gate-delay 2)

;;wire 探头
(define (prob name wire)
  (add-action! wire (lambda ()
                      (newline)
                      (display name)
                      (display " ")
                      (display (current-time the-agenda))
                      (display " New-value = ")
                      (display (get-signal wire)))))

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

  "-----------test code------------"

  (define input-0 (make-wire))
  (define input-1 (make-wire))
  (define output (make-wire))
  (or-gate input-0 input-1 output)
  (prob "or-output" output)
  (set-signal! input-0 1)
  ;在第一次propagate 之前，current-time 都是0
  ;所以不管是add-action! 时候的“默认值设置”调用，还是显式的set-signal! 都被加到同一个time segment 里。 
  (propagate)
  ;在第一次propagate 调用后，我们模拟的“电路世界”的时间前进了，此时我们只有一个time segment，时间是or-gate-delay = 5
  (current-time the-agenda)
  )

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
注意这题的or-gate 和上一题的区别，这里不在需要为参数添加action，因为这里只是单纯的用gate 来组合，而各个具体gate 里面已经对传入的wire 进行了add-action! 操作。"
 (list and-gate or-gate not-gate))


;;export gate definition done previously
(define gate-package (exercise3.29))
(define and-gate (car gate-package))
(define or-gate (cadr gate-package))
(define not-gate (caddr gate-package))

(define (exercise3.30)
  ;;这里用3.28 的or-gate
  
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
  
  "-------test code -----------"
  ;;测试一下半加器
  (define input-0 (make-wire))
  (define input-1 (make-wire))
  (define sum (make-wire))
  (define carry (make-wire))

  (prob "sum" sum)
  (prob "carry" carry)
  (newline)

  (define h-adder (half-adder input-0 input-1 sum carry))
  ""
  (set-signal! input-0 1)
  (propagate)
  (display "current time = ")
  (display (current-time the-agenda))
  (set-signal! input-1 1)
  (propagate)
  )


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

(define (exercise3.32)
  
  (define input-1 (make-wire))
  (define input-2 (make-wire))
  (define output (make-wire))
  (prob "output" output)
  (define and (and-gate input-1 input-2 output))

  (set-signal! input-1 0)
  (set-signal! input-2 1)
  (propagate)
  (newline)
  (set-signal! input-1 1)
  (set-signal! input-2 0)
  (propagate)
  ;;time-segment 里存储action 的顺序对结果有很大影响，对于and-gate，如果segment 按FIFO 存储。
  ;;从0 1 变到 1 0，因为第一个action 是改变input-1信号为 1，此时input-2 还是1，所以and-gate 会输出一次1，第二个action 执行后输出0。也就是说
  ;;在同一个time unit 下and-gate 有两次输出。
  ;;如果改变time-segment 里存储action 的顺序，比如LIFO，则从0,1 -> 1, 0 and-gate 的输出不会有任何变化。
  ;;在同一个time-segment 里改变两次值的现象在真实电路里也是存在的，这里做了讨论https://eli.thegreenplace.net/2007/10/08/sicp-section-334/#comment-124196
  ;;如果对一个逻辑门的两个输入触发间隔大于一次propagate间隔（这里相当于在两个set-signal! 之间插入一次propagate）则会出现在这种逻辑门输出的扰动情况，在电路里称为hazard
  )






