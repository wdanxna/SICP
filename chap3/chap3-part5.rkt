#lang racket
(require sicp)

(define (parallel-execute . procs)
  (map thread-wait
       (map (lambda (proc) (thread proc))
            procs)))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release)
             (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (exercise3.39)
  (define x 10)
  (define s (make-serializer))
  (parallel-execute
   (lambda () (set! x ((s (lambda () (* x x))))))
   (s (lambda () (set! x (+ x 1)))))
"-----------"
  "101: p1 access x at 10 yeild 100; p2 access x at 100 yield 101"
  "121: p2 access x at 10 yeild 11; p2 access x at 11 yeild 121"
  "100: p1 access x at 10,calculate 10*10=100; p2 access x still 10 then set x to 11; p1 continue
with set! set x to 100"
  "p1 的assignment 和access 之间可以被p2 插入"
  x
  )

(define (exercise3.40)
  
  (define (try-untill proc stop)
    (let ((val (proc)))
      (if (stop val)
          'stop
          (try-untill proc stop))))
  
  (define (experiment)
    (let ((x 10))
      (parallel-execute (lambda () (set! x (* x x)) (sleep 0.1))
                        (lambda () (sleep 0.1)(set! x (* x x x))))
      x))

  ;虽然分析出是有这么多结果，但是我实际用这个try-untill 做实验的时候，结果总是1000000.
  (try-untill experiment (lambda (x) (= x 100)))

  "---------"
  "1000000:p1 access x at 10 set x to 100; p2 access x at 100 set x to 1000000; (the other way yeild the same)"
  "100: p1 access x at 10 calculate x*x=100; p2 access x at 10 then set x to x*x*x=1000; p1 then set x to 100"
  "1000: p1 access x at 10 calculate x*x=100; p2 access x at 10 calculate 1000;p1 set x to 100; p2 override
it to 1000"
  "10000: p2 set x to 1000 between 2 access of x of p1, p1 set x to 10 * 1000 = 10000"
  "100000:p1 set x to 100 after the first access of x of p2; p2 then set x to 10 * 100 * 100 = 100000"
  #|
  (define x 10)
  (define s (make-serializer))
  (parallel-execute (s (lambda () (set! x (* x x))))
                    (s (lambda () (set! x (* x x x)))))

  ;"---------"
  ;"1000000:p1 access x at 10 set x to 100; p2 access x at 100 set x to 1000000;"
|#
  "")

(define (exercise3.41)
  "这里读操作不会产生问题。通常读操作不会设计成原子的，除非读取的值是一个复杂的数据结构需要机器执行多个操作，这样为了保证读取过程中内存
一致，需要做原子化")

(define (exercise3.42)
  "感觉复用protected procedure 是可行的具体要看serializer 的实现")


(define (exercise3.43)
  "pitcure here")

(define (exercise3.44)
  "不需要像excange 那样复杂的同步过程，本质区别是withdraw 和deposit 都是原子的，不需要像exchange 一样根据differencce 来决定。")

(define (exercise3.45)
  "不能自动serialize，因为在exchange-serilized 里面会形成死锁，即exchage 会等待withdraw 结束，而withdraw 需要在exchange 结束后才能开始")

(define (exercise3.46)
  "因为test-and-set！ 不是原子的，两个并行进程可以同时读取到false，两个进程都能继续执行下去，mutex 失败")


(define (exercise3.47)
  ;;用两个mutex 试了一下，感觉也是可以的，但是我在c++ 里尝试了一次，各种死锁遂放弃。
  (define (semaphore-in-mutexes n)
    (let ((mutex1 (make-mutex))
          (count n))
      (define (sem m)
        (cond ((eq? m 'wait)
               (mutex1 'aquire)
               (if (= count 0)
                   (begin
                     (mutex1 'release)
                     (sem 'wait))
                   (begin
                     (set! count (- count 1))
                     (mutex1 'release))))
              ((eq? m 'signal)
               (mutex1 'aquire)
               (if (= count n)
                   (mutex1 'release)
                   (begin
                     (set! count (+ count 1))
                     (mutex1 'release))))))
      sem))

  #|这题不是这个意思 =_=
  (define (semaphore n)
    (let ((count n)
          (cell (list false)))
      (define (sem m)
        (cond ((eq? m 'wait)
               (if (test-and-set! cell)
                   (sem 'wait)
                   (if (= count 0)
                       (begin
                         (set-car! cell false)
                         (sem 'wait))
                       (begin
                        (set! count (- count 1))
                        (set-car! cell false)))))
              ((eq? m 'signal)
               (if (test-and-set! cell)
                   (m 'signal)
                   (if (= count n)
                       (set-car! cell false)
                       (begin
                         (set! count (+ count 1))
                         (set-car! cell true)))))))
      sem))

|#


  ;;是这个意思-_-||
  (define (semaphore n)
    ;;assume atomic
    (define (test-and-set! cell)
      (if (= (car cell) 0)
          true
          (begin
            (set-car! cell (- (car cell) 1))
            false)))

    ;;assume atomic
    (define (clear! cell)
      (if (< (car cell) n)
          (set-car! cell (+ (car cell) 1))))

    (define (sem m)
      (let ((cell (list n)))
        (cond ((eq? m 'wait)
               (if (test-and-set! cell)
                   (m 'wait)))
              ((eq? m 'signal)
               (clear! cell)))))
    sem)
  "")


(define (exercise3.48)
  (define (make-account-and-serializer balance id)
      (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))
      (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
      (let ((balance-serializer (make-serializer)))
        (define (dispatch m)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'balance) balance)
                ((eq? m 'serializer) balance-serializer)
                ((eq? m 'id-number) id)
                (else (error "Unknown request: MAKE-ACCOUNT" m))))
        dispatch))

  (define (exchange account1 account2)
    (let ((difference (- (account1 'balance)
                         (account2 'balance))))
      ((account1 'withdraw) difference)
      ((account2 'deposit) difference)))
  
  (define (serialized-exchange account1 account2)
    (let ((serializer1 (account1 'serializer))
          (serializer2 (account2 'serializer)))
      (if (> (account1 'id-number) (account2 'id-number));;从小到大
          ((serializer1 (serializer2 exchange))
           account1
           account2)
          (serialized-exchange account2 account1))));;否则就颠倒过来

  "serialized-exchange 其实是嵌套的mutex，将共享对象按照一定顺序加锁可以避免死锁，原因是如果保证m2 上锁前必须锁上m1,则不会出现获取m1 之前m2 已经被锁上的情况")

(define (exercise3.49)
  "题目的提示已经说的很明确，对于预先知道要访问哪些共享资源的时候，可以使用给每个资源一个序号，从小到大安排加锁顺序的方式避免死锁, 但是如果事先不知道，比如先锁了4 号资源，但是突然发现要锁1 号。")


