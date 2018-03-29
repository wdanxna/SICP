#lang planet neil/sicp
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; a 加上b 的绝对值，其中运算符是一个组合式