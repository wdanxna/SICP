#lang planet neil/sicp
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))
;执行上述过程，在应用序下会死循环，因为应用序要首先对参数求值，而过程p 是无出口的递归定义
;正则序将返回0