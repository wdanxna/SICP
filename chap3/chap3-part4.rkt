#lang sicp

;-------- connectors ------
(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignore))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: CONNECTOR" request))))
    me))

(define (for-each-except exception proc seq)
  (cond ((null? seq) 'done)
        ((eq? (car seq) exception)
         (for-each-except exception proc (cdr seq)))
        (else
         (proc (car seq))
         (for-each-except exception proc (cdr seq)))))

;;tells whether the connector has a value
(define (has-value? connector)
  (connector 'has-value?))

;;return the connector's current value
(define (get-value connector)
  (connector 'value))

;;indicates that the informant is requesting the connector to
;;set its value to the new value
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

;;tells the connector that the retractor is requesting it to
;;forget its value
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

;;tell the connector to participate in the new constraint
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;;tells the given constraint that the connector has a value
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

;;tells the constraint that the connector has lost its value
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

;;------------ constraint -----------
;adder
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

;multiplier
(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product)
                          (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: MULTIPLIER" request))))
  ;;I forget to connect connector! it took me 30 min to debug!
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

;constant
(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)


;;------ spetial connector -----
;prob
(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ") (display name)
    (display " = ") (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: PROBE" request))))
  ;;don't forget to connect the connector!!!!!
  (connect connector me)
  me)




(define (exercise3.33)

  (define (averager a b c)
    (let ((x (make-connector))
          (y (make-connector)))
      (let ((constaint-2 (constant 2 y)))
        (adder a b x)
        (multiplier c y x)
        'done)))
  
"------ test code -----"
  (define a (make-connector))
  (define b (make-connector))
  (define c (make-connector))
  (define p (probe "avg" c))

  (averager a b c)
  (set-value! a 1 'user)
  (set-value! b 2 'user)

  ;;this would trigger error for value contradiction
  ;(set-value! c 4 'user)

  ;;forget a, then set avg to get a
  (forget-value! a 'user)
  (define p2 (probe "a" a))
  (set-value! c 4 'user))

(define (exercise3.34)

  (define (squarer a b)
    (multiplier a a b))

  "---test code ----"

  (define a (make-connector))
  (define b (make-connector))
  (define p (probe "b" b))
  (squarer a b)

  ;;eventhough we can calculate b with this direction
  (set-value! a 2 'user)

  ;;we can't yeild sqrt from the orther direction
  (forget-value! a 'user)
  (probe "a" a)
  (set-value! b 4 'user);probe 'a' will not yeild
  )

(define (exercise3.35)

  ;;顺便复习一下不动点求平方根
  ;;f(x)的不动点是一个x 满足 x = f(x)
  ;;可以通过对一个初始值反复应用f(x) 当结果不再变动时即为不动点
  (define (fix-point f first-guess)
    (define (good-enough? a b)
      (< (abs (- a b)) 0.000001))
    (let ((next-value (f first-guess)))
      (if (good-enough? first-guess next-value)
          next-value
          (fix-point f next-value))))

  ;;针对部分函数求不动点不收敛的情况，我们需要采用平均阻尼技术
  ;;平均阻尼即对更新函数做修改，让他产生一个初始值和更新值的平均数
  (define (average-damp f)
    (lambda (x)
      (/ (+ (f x) x)
         2)))

  ;;采用平均阻尼+不动点的方式求sqrt
  ;;因为：a^2 = b；我们关心a 的值
  ;;两边除以a 得到: a = b/a
  ;;设f(x) = b/x；那么求平方根就是求f(x) 的不动点。
  (define (sqrt x)
    (fix-point (average-damp (lambda (y) (/ x y))) 1.0))

  ;;Constraint box
  (define (squarer a b)
    (define (process-new-value)
      (if (has-value? b)
          (if (< (get-value b) 0)
              (error "square less then 0: SQUARER" (get-value b))
              (set-value! a
                          (sqrt (get-value b))
                          me))
          (if (has-value? a)
              (set-value! b
                          (* (get-value a) (get-value a))
                          me))))
    (define (process-forget-value)
      (forget-value! a me)
      (forget-value! b me)
      (process-new-value))
    (define (me request)
      (cond ((eq? request 'I-have-a-value) (process-new-value))
            ((eq? request 'I-lost-my-value) (process-forget-value))
            (else (error "Unknown request: SQUARER" request))))
    (connect a me)
    (connect b me)
    me)

  "----- test code ----"
  ;;测试一下sqrt，好像没问题
  (sqrt 16)

  (define a (make-connector))
  (define b (make-connector))
  (squarer a b)
  (probe "b" b)
  (set-value! a 2 'user);;b = 4

  (forget-value! a 'user);;b = ?
  (probe "a" a)
  (set-value! b 4 'user);;a = 2
  )

(define (exercise3.37)
  (define (c+ a b)
    (let ((c (make-connector)))
      (adder a b c)
      c))
  (define (c* x y)
    (let ((z (make-connector)))
      (multiplier x y z)
      z))
  (define (c/ x y)
    (let ((z (make-connector)))
      (multiplier z y x)
      z))
  (define (cv x)
    (let ((y (make-connector)))
      (constant x y)
      y))
  
  (define (celsius-fahrenheit-converter x)
    (c+ (c* (c/ (cv 9) (cv 5))
            x)
        (cv 32)))
  ""
  (define C (make-connector))
  (define F (celsius-fahrenheit-converter C))
  
  "-----test code---"
  (probe "F" F)
  (probe "C" C)
  (set-value! C 1 'user)
  (forget-value! C 'user)
  (set-value! F 33.8 'user)
  )

