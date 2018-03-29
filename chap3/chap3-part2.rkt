#lang sicp

(define (exercise3.19)
    ;;use 2 pointer
  (define (cycle? x)
    ""
    (define p1 x)
    (define p2 x)

    (define (iter first)
      (cond ((not (pair? x)) false)
            ((not (pair? p1)) false)
            ((not (pair? p2)) false)
            ((and (not first)
                  (eq? p1 p2)) true)
            (else (set! p1 (cdr p1))
                  (if (pair? (cdr p2))
                      (set! p2 (cddr p2))
                      false)
                  (iter false))))
    (iter true))
  
  "--------------"
  (define a (cons 'a 'b))
  (define b (cons 'c a))
  (set-car! a b)
  (cycle? a))


(define (exercise3.20)
  ""
  (define (cons x y)
    (define (set-x! v) (set! x v))
    (define (set-y! v) (set! y v))

    (define (dispatch m)
      (cond ((eq? m 'car) x)
            ((eq? m 'cdr) y)
            ((eq? m 'set-car!) set-x!)
            ((eq? m 'set-cdr!) set-y!)))
    dispatch)

  (define (car x) (x 'car))
  (define (cdr x) (x 'cdr))
  (define (set-car! x v) ((x 'set-car!) v))
  (define (set-cdr! x v) ((x 'set-cdr!) v))
  "--------------"
  (define x (cons 1 2))
  (define z (cons x x))
  (set-car! (cdr z) 17)
  (car x)
  )


(define (front-ptr queue)
  (car queue))

(define (rear-ptr queue)
  (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

;;actual queue operation
(define (empty-queue? queue)
  (null? (front-ptr queue)))

;;constructor
(define (make-queue)
  (cons '() '()))

;;selector
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with empty queue" queue)
      (car (front-ptr queue))))

;;mutator
(define (insert-queue! queue item)
  (let ((new-item (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-item)
           (set-rear-ptr! queue new-item)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-item)
           (set-rear-ptr! queue new-item)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE called with empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


(define (exercise3.21)
  "这题一开始我还以为会很复杂，因为想到要从front 指针开始遍历直到rear 指针结束，写到
一半发现完全这不就是print list吗？用自带的display 就可以搞定啊-_-"
  (define (print-queue queue)
    (cond ((empty-queue? queue)
           (display '()))
          (else
           (display (front-ptr queue)))))

  "----------"
  (define x (make-queue))
  (insert-queue! x 'a)
  (insert-queue! x 'b)
  (delete-queue! x)
  (delete-queue! x)
  (print-queue x))


(define (exercise3.22)
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
              ((eq? m 'print) (print))
              (else (error "Invalid message for queue" m))))
      dispatch))
  "--------------------"

  (define queue (make-queue))
  ((queue 'insert!) 'a)
  ((queue 'insert!) 'b)
  (queue 'delete!)
  (queue 'delete!)
  (queue 'empty?)
  (queue 'print)
  )



(define (exercise3.23)
  (define (make-deque)
    (let ((front-ptr '())
          (rear-ptr '()))

      (define (empty-deque?)
        (or (null? front-ptr)
             (null? rear-ptr)))

      (define (front-deque)
        front-ptr)
      (define (rear-deque)
        rear-ptr)

      (define (front-insert-deque! item)
        (let ((new-item (list item '() '())))
          (cond ((empty-deque?)
                 (set! front-ptr new-item)
                 (set! rear-ptr new-item))
                (else
                 (set-car! (cddr new-item) front-ptr)
                 (set-car! (cdr front-ptr) new-item)
                 (set! front-ptr new-item)))))

      (define (rear-insert-deque! item)
        (let ((new-item (list item '() '())))
          (cond ((empty-deque?)
                 (set! front-ptr new-item)
                 (set! rear-ptr new-item))
                (else
                 (set-car! (cddr rear-ptr) new-item)
                 (set-car! (cdr new-item) rear-ptr)
                 (set! rear-ptr new-item)))))

      (define (front-delete-deque!)
        (cond ((empty-deque?)
              (error "DELETE call with empty deque"))
              (else
               (set! front-ptr (caddr front-ptr))
               (if (not (null? front-ptr))
                   (set-car! (cdr front-ptr) '())
                   ""))))

      (define (rear-delete-deque!)
        (cond ((empty-deque?)
               (error "DELETE call with empty deque"))
              (else
               (set! rear-ptr (cadr rear-ptr))
               (if (not (null? rear-ptr))
                   (set-car! (cddr rear-ptr) '())
                   ""))))

      (define (print-deque)
        (define (print-iter x)
          (if (null? x)
              (display ")")
              (begin
                (display (car x))
                (display " ")
                (print-iter (caddr x)))))
        (cond ((empty-deque?)
               (display '()))
              (else
               (display "(")
               (print-iter front-ptr))))
      ""
      (define (dispatch m)
        (cond ((eq? m 'empty-deque?) (empty-deque?))
              ((eq? m 'front-insert-deque!) front-insert-deque!)
              ((eq? m 'rear-insert-deque!) rear-insert-deque!)
              ((eq? m 'front-delete-deque!) (front-delete-deque!))
              ((eq? m 'rear-delete-deque!) (rear-delete-deque!))
              ((eq? m 'print-deque) (print-deque))
              ((eq? m 'front-ptr) front-ptr)
              ((eq? m 'rear-ptr) rear-ptr)
              (else (error "INVALID message send to deque" m))))
      dispatch))
  "--------testing code-----------"
  (define deque (make-deque))
  ((deque 'front-insert-deque!) 'a)
  ((deque 'front-insert-deque!) 'b)
  ((deque 'rear-insert-deque!) 'c)
  (deque 'print-deque)
  (newline)
  (deque 'front-delete-deque!)
  (deque 'front-delete-deque!)
  (deque 'front-delete-deque!)
  (deque 'print-deque);should be empty here
  (newline)
  ((deque 'rear-insert-deque!) 'a)
  ((deque 'rear-insert-deque!) 'b)
  (deque 'front-delete-deque!)
  ((deque 'front-insert-deque!) 'a)
  (deque 'rear-delete-deque!)
  (deque 'front-delete-deque!)
  (deque 'print-deque)
  "这题做了很长时间，因为花了很长时间来debug，原因是item 的数据结构变复杂了
queue 的item 很简单(cons item next)；deque 的因为要从尾部删除，所以每个item 都要保存前一个节点的指针
(list item prev next)；这就导致insert 和delete 操作都要格外小心的维护这两个指针")




;;2D Table
(define (equal? a b)
  (cond
    ((null? a) (null? b))
    ((and (number? a) (number? b)) (= a b))
    ((symbol? a) (eq? a b))
    ((and (pair? a) (pair? b))
     (if (equal? (car a) (car b))
         (equal? (cdr a) (cdr b))
         false))
    (else false)))

(define (make-table)
  (let ((local-table '('*table*)))
      
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 subtable)))
              (if record
                  (cdr record)
                  false))
            false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 subtable)))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2
                                        value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table))))))
    "------"
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))


(define (exercise3.24)

  (define (equal? a b)
    (cond
      ((null? a) (null? b))
      ((and (number? a) (number? b)) (= a b))
      ((symbol? a) (eq? a b))
      ((and (pair? a) (pair? b))
       (if (equal? (car a) (car b))
           (equal? (cdr a) (cdr b))
           false))
      (else false)))

  (define (make-table same-key?)
    (let ((local-table '('*table*)))

      (define (assoc key table)
        (if (null? table)
            false
            (if (same-key? key (caar table))
                (car table)
                (assoc key (cdr table)))))
      
      (define (lookup key-1 key-2)
        (let ((subtable (assoc key-1 (cdr local-table))))
          (if subtable
              (let ((record (assoc key-2 (cdr subtable))))
                (if record
                    (cdr record)
                    false))
              false)))

      (define (insert! key-1 key-2 value)
        (let ((subtable (assoc key-1 (cdr local-table))))
          (if subtable
              (let ((record (assoc key-2 (cdr subtable))))
                (if record
                    (set-cdr! record value)
                    (set-cdr! subtable
                          (cons (cons key-2
                                      value)
                                (cdr subtable)))))
              (set-cdr! local-table
                        (cons (list key-1
                                    (cons key-2 value))
                              (cdr local-table))))))
      "------"
      (define (dispatch m)
        (cond ((eq? m 'lookup-proc) lookup)
              ((eq? m 'insert-proc!) insert!)
              (else (error "Unknown operation: TABLE" m))))
      dispatch))
  "-----------------"

  (define table-1 (make-table (lambda (a b)
                                (eq? a b))))
  ((table-1 'lookup-proc) 'level1 'level2)
  ((table-1 'insert-proc!) 'level1 'level2 'value)
  ((table-1 'lookup-proc) 'level1 'level2)
  ((table-1 'insert-proc!) 'level1 '(1) 'array-as-key)
  ((table-1 'lookup-proc) 'level1 '(1));this is false, because same-key is not suit
  
  (define table-2 (make-table (lambda (a b)
                                 (equal? a b))))
  ((table-2 'insert-proc!) 'level1 '(1) 'array-as-key)
  ((table-2 'lookup-proc) 'level1 '(1));now entry can be located because we use equal
  )


(define (exercise3.25)
  (define (equal? a b)
    (cond
      ((null? a) (null? b))
      ((and (number? a) (number? b)) (= a b))
      ((symbol? a) (eq? a b))
      ((and (pair? a) (pair? b))
       (if (equal? (car a) (car b))
           (equal? (cdr a) (cdr b))
           false))
      (else false)))
  
  (define (make-table)
    (let ((local-table '(*table*)))

      (define (assoc key table)
        (if (null? table)
            false
            (if (equal? key (caar table))
                (car table)
                (assoc key (cdr table)))))

      (define (lookup keys)
        (define (lookup-iter keys table)
          (let ((subtable (assoc (car keys) (cdr table))))
            (if subtable
                (if (null? (cdr keys))
                    (cadr subtable)
                    (lookup-iter (cdr keys) (cdr subtable)))
                false)))
        (lookup-iter keys local-table))

      (define (insert! keys value)
        (define (insert-iter! keys table)
          (let ((subtable (assoc (car keys) (cdr table))))
            (if subtable
                (if (null? (cdr keys))
                    (set-car! (cdr subtable) value)
                    (insert-iter! (cdr keys) (cdr subtable)))
                (begin
                  (set-cdr! table
                            (cons
                             (list (car keys) false)
                             (cdr table)))
                  (if (null? (cdr keys))
                      (set-car! (cdadr table) value)
                      (insert-iter! (cdr keys) (cdadr table)))))))
        (insert-iter! keys local-table))
      "-----------------"
      (define (dispatch m)
        (cond ((eq? m 'lookup-proc) lookup)
              ((eq? m 'insert-proc!) insert!)
              (else (error "Unknown Operation send to table" m))))
      dispatch))
  
  "-------------test code below----------------"
  "这道题也花了很长时间,主要精力花在仔细的研究多维表的递归结构,比如subtable 从哪里开始?还有查询和插入的递归过程
,怎么依次处理每个key,怎么精密的定义递归过程。骚操作在insert! 找不到subtable 情况下的递归处理"
  (define tb (make-table))
  ((tb 'insert-proc!) '(a b) 'v-a-b)
  ((tb 'lookup-proc) '(a b));'v-a-b
  ;((tb 'lookup-proc) '('a));#f
  ;((tb 'insert-proc!) '('a) 'v-a)
  ;((tb 'lookup-proc) '('a));'v-a
  ;((tb 'insert-proc!) '('a 'b 'c 'd) 'v-a-b-c-d)
  ;((tb 'lookup-proc) '('a 'b 'c));#f
  ;((tb 'lookup-proc) '('a 'b 'c 'd));'v-a-b-c-d
  ;((tb 'insert-proc!) '('c 'd 'e) 'v-c-d-e)
  ;((tb 'lookup-proc) '('c 'd 'e));'v-c-d-e
  ;((tb 'insert-proc!) '('c 'd) 'v-c-d)
  ;((tb 'lookup-proc) '('c 'd));'v-c-d
  ;((tb 'insert-proc!) '('c 'd) 'v-c-d-2)
  ;((tb 'lookup-proc) '('c 'd));'v-c-d-2
  )



(define (exercise3.26)
  (define (equal? a b)
    (cond
      ((null? a) (null? b))
      ((and (number? a) (number? b)) (= a b))
      ((symbol? a) (eq? a b))
      ((and (pair? a) (pair? b))
       (if (equal? (car a) (car b))
           (equal? (cdr a) (cdr b))
           false))
      (else false)))
  
  (define (make-table)
    (let ((local-table (list '*table* '())))

      (define (make-tree key value subtable left right)
        (list key value subtable left right))
      
      (define (left-branch tree)
        (cdddr tree))

      (define (right-branch tree)
        (cddddr tree))

      (define (set-left-branch! tree branch)
        (set-car! (cdddr tree) branch))

      (define (set-right-branch! tree branch)
        (set-cdr! (cdddr tree) branch))

      (define (set-subtable! tree table)
        (set-car! (cddr tree) table))

      (define (set-value! tree value)
        (set-car! (cdr tree) value))

      (define (key-tree tree)
        (car tree))

      (define (value-tree tree)
        (cadr tree))

      (define (subtable-tree tree)
        (cddr tree))
      "---------tree---------------"
      
      (define (symbol>? a b)
        (let ((str-a (symbol->string a))
              (str-b (symbol->string b)))
          (string>? str-a str-b)))
      
      (define (symbol<? a b)
        (let ((str-a (symbol->string a))
              (str-b (symbol->string b)))
          (string<? str-a str-b)))

      (define (lookup keys)
        (define (lookup-iter keys table)
          (let ((tree (car table)))
            (cond ((null? tree) false)
                  ((symbol>? (car keys) (key-tree tree))
                   (lookup-iter keys (right-branch tree)))
                  ((symbol<? (car keys) (key-tree tree))
                   (lookup-iter keys (left-branch tree)))
                  (else
                   (if (null? (cdr keys))
                       (value-tree tree)
                       (lookup-iter (cdr keys) (subtable-tree tree)))))))
        (lookup-iter keys (cdr local-table)))

      (define (insert! keys value)
        (define (insert-iter! keys table)
          (let ((tree (car table)))
             (cond ((null? tree)
                    (let ((new-tree (make-tree (car keys) false '() '() '())))
                      (set-car! table new-tree)
                      (if (null? (cdr keys))
                          (set-value! new-tree value)
                          (insert-iter! (cdr keys) (subtable-tree new-tree)))))
                   ((null? (cdr keys))
                    (set-value! tree value))
                   ((symbol<? (car keys) (key-tree tree))
                    (insert-iter! keys (left-branch tree)))
                   ((symbol>? (car keys) (key-tree tree))
                    (insert-iter! keys (right-branch tree)))
                   (else
                    (insert-iter! (cdr keys) (subtable-tree tree))))))
        (insert-iter! keys (cdr local-table)))
      
      "-----------------"
      (define (dispatch m)
        (cond ((eq? m 'lookup-proc) lookup)
              ((eq? m 'insert-proc!) insert!)
              (else (error "Unknown Operation send to table" m))))
      dispatch))
  
  "-------------test code below----------------"
  (define tb (make-table))
  ((tb 'insert-proc!) '(a b) 'v-a-b)
  ((tb 'lookup-proc) '(a b));'v-a-b
  ((tb 'lookup-proc) '(a));#f
  ((tb 'insert-proc!) '(a) 'v-a)
  ((tb 'lookup-proc) '(a));'v-a
  ((tb 'insert-proc!) '(a b c d) 'v-a-b-c-d)
  ((tb 'lookup-proc) '(a b c));#f
  ((tb 'lookup-proc) '(a b c d));'v-a-b-c-d
  ((tb 'insert-proc!) '(c d e) 'v-c-d-e)
  ((tb 'lookup-proc) '(c d e));'v-c-d-e
  ((tb 'insert-proc!) '(c d) 'v-c-d)
  ((tb 'lookup-proc) '(c d));'v-c-d
  ((tb 'insert-proc!) '(c d) 'v-c-d-2)
  ((tb 'lookup-proc) '(c d));'v-c-d-2
  "这题也花了很长时间，写起来很痛苦，主要时间浪费在一开始没有想清楚数据结构要怎么安排，
开始我直接把table 实现成(list key value left right).后面发现如果table 是个单纯的tree 的话就变成一维表了（或者说是带key 的tree-set），
后面把table 改成(list key value subtable left right) 其中subtable 是下一维度的tree，而left right 则是当前维度的‘龙骨’。
为了让递归过程通用，要让每个table 都从car 开始，所以local-table 要加一个dummpy 节点，第一维的tree 是dummpy node 的car。
除此之外，递归过程要十分注意当前层传给下一层的数据结构是否满足闭包，不然就会出现各种奇怪和让人沮丧的问题，
比如tree 的selector：subtable、left-branch、right-branch 为什么这样实现，都是为了在lookup 和insert! 递归过程中保持闭包特性。")


(define (exercise3.27)
  "------table-------"
  (define (equal? a b)
    (cond
      ((null? a) (null? b))
      ((and (number? a) (number? b)) (= a b))
      ((symbol? a) (eq? a b))
      ((and (pair? a) (pair? b))
       (if (equal? (car a) (car b))
           (equal? (cdr a) (cdr b))
           false))
      (else false)))
  
  (define (make-table)
    (let ((local-table '(*table*)))

      (define (assoc key table)
        (if (null? table)
            false
            (if (equal? key (caar table))
                (car table)
                (assoc key (cdr table)))))

      (define (lookup keys)
        (define (lookup-iter keys table)
          (let ((subtable (assoc (car keys) (cdr table))))
            (if subtable
                (if (null? (cdr keys))
                    (cadr subtable)
                    (lookup-iter (cdr keys) (cdr subtable)))
                false)))
        (lookup-iter keys local-table))

      (define (insert! keys value)
        (define (insert-iter! keys table)
          (let ((subtable (assoc (car keys) (cdr table))))
            (if subtable
                (if (null? (cdr keys))
                    (set-car! (cdr subtable) value)
                    (insert-iter! (cdr keys) (cdr subtable)))
                (begin
                  (set-cdr! table
                            (cons
                             (list (car keys) false)
                             (cdr table)))
                  (if (null? (cdr keys))
                      (set-car! (cdadr table) value)
                      (insert-iter! (cdr keys) (cdadr table)))))))
        (insert-iter! keys local-table))
     
      (define (dispatch m)
        (cond ((eq? m 'lookup-proc) lookup)
              ((eq? m 'insert-proc!) insert!)
              (else (error "Unknown Operation send to table" m))))
      dispatch))
  "------ table above -----------"
  (define (lookup key table)
    ((table 'lookup-proc) (list key)))

  (define (insert! key val table)
    ((table 'insert-proc!) (list key) val))
  
  (define (fib n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1))
                   (fib (- n 2))))))

  (define (memoize f)
    (let ((table (make-table)))
      (lambda (x)
        (let ((previously-computed-result
               (lookup x table)))
          (or previously-computed-result
              (let ((result (f x)))
                (insert! x result table)
                result))))))

  (define memo-fib
    (memoize
     (lambda (n)
       (cond ((= n 0) 0)
             ((= n 1) 1)
             (else (+ (memo-fib (- n 1))
                      (memo-fib (- n 2))))))))

  "---------test code below -------------"
  ;(fib 40);102334155
  ;(memo-fib 40)
  ((memoize fib) 40);this can still work, but its actually memorize single value (fib n), noting else.
  )