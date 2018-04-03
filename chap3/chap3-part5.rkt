#lang sicp

(define (parallel-execute ...p)
  "")
(define (make-serializer p)
  (lambda ()
    ""))

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
  )

(define (exercise3.40)
  (define x 10)
  (parallel-execute (lambda () (set! x (* x x)))
                    (lambda () (set! x (* x x x))))

  "---------"
  "1000000:p1 access x at 10 set x to 100; p2 access x at 100 set x to 1000000; (the other way yeild the same)"
  "100: p1 access x at 10 calculate x*x=100; p2 access x at 10 then set x to x*x*x=1000; p1 then set x to 100"
  "1000: p1 access x at 10 calculate x*x=100; p2 access x at 10 calculate 1000;p1 set x to 100; p2 override
it to 1000"
  "10000: p2 set x to 1000 between 2 access of x of p1, p1 set x to 10 * 1000 = 10000"
  "100000:p1 set x to 100 after the first access of x of p2; p2 then set x to 10 * 100 * 100 = 100000"

  (define x 10)
  (define s (make-serializer))
  (parallel-execute (s (lambda () (set! x (* x x))))
                    (s (lambda () (set! x (* x x x)))))

  "---------"
  "1000000:p1 access x at 10 set x to 100; p2 access x at 100 set x to 1000000;")

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

