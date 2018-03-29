#lang planet neil/sicp
(define (close-enough? a b)
  (< (abs (- a b)) 0.001))

(define (average a b)
  (/ (+ a b) 2.0))

(define (positive? x)
  (> x 0))

(define (negative? x)
  (< x 0))

(define (search f neg pos)
	(let ((mid-point (average neg pos)))
		(if (close-enough? neg pos)
			mid-point
			(let ((mid-value (f mid-point)))
				(cond ((positive? mid-value)
					(search f neg mid-point))
					((negative? mid-value)
					(search f mid-point pos))
(else mid-point))))))


(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (positive? a-value) (negative? b-value))
           (search f b a))
          ((and (positive? b-value) (negative? a-value))
           (search f a b))
          (else
           (error "value are not of opposits sign" a b)))))


(define (fix-point f first-guess)
  (define (good-enough? a b)
    (< (abs (- a b)) 0.00001))
  (let ((val (f first-guess)))
    (if (good-enough? first-guess val)
        val
        (fix-point f val))))

(define (sqrt x)
  (fix-point (lambda (y) (/ (+ y (/ x y)) 2.0)) 1.0))

(define (average-damp f)
  (lambda (y) (average y (f y))))

(define (sqrt2 x)
  (fix-point (average-damp (lambda (y) (/ x y))) 1.0))

(define dx 0.00001)
(define (deriv f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (newton-transform f)
  (lambda (x) (- x (/ (f x) ((deriv f) x)))))

(define (newton-method f)
  (fix-point (newton-transform f) 1.0))

(define (square x)
  (* x x))

(define (sqrt3 x)
  (newton-method (lambda (y) (- (square y) x))))


(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (*(numer y) (denom x)))
            (* (denom x) (denom y))))

;;segment & point
(define (make-segment p q) (cons p q))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment s)
  (make-point (/ (+ (x-point (start-segment s)) (x-point (end-segment s))) 2)
              (/ (+ (y-point (start-segment s)) (y-point (end-segment s))) 2)))

;;rectangle definition1
(define (make-rect p q) (cons p q))
(define (top-left-rect r) (car r))
(define (bottom-right-rect r) (cdr r))
(define (width-rect r) (- (x-point (bottom-right-rect r)) (x-point (top-left-rect r))))
(define (height-rect r) (- (y-point (top-left-rect r)) (y-point (bottom-right-rect r))))

;;rectangle definition2
(define (make-rect2 origin size) (cons origin size))
(define (width-rect2 r) (car (cdr r)))
(define (height-rect2 r) (cdr (cdr r)))
(define (top-left-rect2 r) (car r))
(define (bottom-right-rect2 r)
  (make-point (+ (x-point (top-left-rect2 r)) (width-rect2 r))
              (- (y-point (top-left-rect2 r)) (height-rect2 r))))
;;rectangle operation
(define (circum-rect r)
  (+ (* 2 (width-rect r))
     (* 2 (height-rect r))))

(define (area-rect r)
  (* (width-rect r)
     (height-rect r)))


;;2.17
(define (last-pair list)
  (define (pair-iter list last)
    (if (null? list)
        (cons last nil)
        (pair-iter (cdr list) (car list))))
  (pair-iter list nil))

(define (last-pair2 list)
  (if (null? (cdr list))
      list
      (last-pair2 (cdr list))))

;;2.18
(define (reverse list)
  (define (reverse-iter list ret)
    (if (null? (cdr list))
        (cons (car list) ret)
        (reverse-iter (cdr list) (cons (car list) ret))))
  (reverse-iter list nil))

(define (list-ref list n)
  (if (= n 0)
      (car list)
      (list-ref (cdr list) (- n 1))))

(define (length list)
  (if (null? list)
      0
      (+ 1 (length (cdr list)))))

(define (length2 list)
  (define (length-iter list cnt)
    (if (null? list)
        cnt
        (length-iter (cdr list) (+ cnt 1))))
  (length-iter list 0))

(define (append a b)
  (if (null? a)
      b
      (cons (car a) (append (cdr a) b))))

(define (reverse2 list)
  (define (reverse-iter list ret)
  (if (null? list)
      ret
  (reverse-iter (cdr list) (cons (car list) ret))))
  (reverse-iter list '()))

(define (scale-list list n)
  (if (null? list)
      nil
      (cons (* 3 (car list)) (scale-list (cdr list) n))))

(define (map proc list)
  (if (null? list)
      nil
      (cons (proc (car list)) (map proc (cdr list)))))

(define (scale-list2 list n)
  (map (lambda (x) (* x n)) list))

(define (for-each proc list)
  (if (not (null? list))
      ((lambda (x) (proc (car list)) (for-each proc (cdr list))) 0)))

(define (print-list list)
  (cond ((null? list) (display ")"))
        ((pair? (car list)) 
         (display "(") 
         (print-list (car list)) 
         (print-list (cdr list)))
        (else (display (car list)) (print-list (cdr list)))))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((pair? (car tree)) (+ (count-leaves (car tree))
                               (count-leaves (cdr tree))))
        (else (+ 1 (count-leaves (cdr tree))))))

(define (count-leaves2 l)
  (cond ((null? l) 0)
        ((not (pair? l)) 1)
        (else (+ (count-leaves2 (car l))
                 (count-leaves2 (cdr l))))))

(define (scale-tree tree n)
  (cond ((null? tree) nil)
        ((not (pair? tree) (* n tree)))
        (else (cons (scale-tree (car tree) n)
                    (scale-tree (cdr tree) n)))))

(define (scale-tree2 tree n)
  (map (lambda (x)
         (if (pair? x)
             (scale-tree2 x n)
             (* n x)))
       tree))

(define (map-tree proc tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (proc tree))
        (else (cons (map-tree proc (car tree))
                    (map-tree proc (cdr tree))))))

(define (fib n)
  (define (fib-iter a b n)
    (if (= n 0)
        a
        (fib-iter b (+ a b) (- n 1))))
  (fib-iter 0 1 n))

(define (sum-of-odd-square tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree)
             (square tree)
             0))
        (else (+ (sum-of-odd-square (car tree))
                 (sum-of-odd-square (cdr tree))))))

(define (even-fibs n)
  (if (= n 0)
      nil
      (let ((f (fib n)))
        (if (even? f)
            (cons f
                  (even-fibs (- n 1)))
            (even-fibs (- n 1))))))

(define (filter predicate sequence)
  (if (null? sequence) 
       nil
       (if (predicate (car sequence))
           (cons (car sequence) 
                 (filter predicate (cdr sequence)))
           (filter predicate (cdr sequence)))))

(define (accumulate op init sequence)
  (if (null? sequence) 
      init
      (op (car sequence)
         (accumulate op init (cdr sequence)))))

(define (enumerate-tree tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree)) 
                        (enumerate-tree (cdr tree))))))

(define (sum-of-odd-square2 tree)
  (let ((leaves (enumerate-tree tree)))
    (let ((odd-leaves (filter (lambda (x) (odd? x))
                              leaves)))
      (accumulate (lambda (a b) (+ (square a) b)) 0 odd-leaves))))

(define (map2 p sequence)
  (accumulate (lambda (a b) (cons (p a) b)) nil sequence))

(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length3 seq)
  (accumulate (lambda (a b) (+ b 1)) 0 seq))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff (* x higher-terms))) 0 coefficient-sequence))

(define (count-leaves3 t)
  (accumulate (lambda (current-tree rest-leaves-count)
                (cond ((null? current-tree) 0)
                      ((not (pair? current-tree)) (+ 1 rest-leaves-count))
                      (else (+ (count-leaves3 current-tree) rest-leaves-count)))) 0 t))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (accumulate-n * 1 (list v w))))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col)
                  (dot-product row col)) cols)) m)))

;;2.39
(define (fold-right op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (fold-right op init (cdr sequence)))))

(define (fold-left op init sequence)
  (define (fold-iter acc seq)
    (if (null? seq)
        acc
        (fold-iter (op acc (car seq))
                   (cdr seq))))
  (fold-iter init sequence))

(define (reverse3 sequence)
  (fold-right (lambda (x y)
                (if (null? y)
                    (list x)
                    (append y (list x)))) nil sequence))

(define (reverse4 sequence)
  (fold-left (lambda (x y)
               (cons y x)) nil sequence))

(define (enumerate-interval start end)
  (if (> start end)
      nil
      (cons start
            (enumerate-interval (+ start 1) end))))

(define (nested-pairs n)
  (fold-right append nil
              (map (lambda (i)
                     (map (lambda (j)
                            (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;(define (prime-sum? pair)
;  (prime? (+ (car pair) (cdr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cdr pair) (+ (car pair) (cdr pair))))

(define (remove a seq)
  (filter (lambda (x) (not (= x a))) seq))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                  (map (lambda (set) (cons x set))
                       (permutations (remove x s)))) s)))

(define (in x seq)
  (if (null? seq)
      #f
      (if (= x (car seq))
          #t
          (in x (cdr seq)))))

(define (at i seq)
  (if (null? seq)
      (error "index out of bounds, size of sequence is" (+ i 1))
      (if (= i 0)
          (car seq)
          (at (- i 1) (cdr seq)))))

(define board (list 2 6 1 7 4 0 3))

;(define (remove-last seq)
;  (reverse (cdr (reverse seq))))
(define (remove-last seq)
  (if (null? (cdr seq))
      nil
      (cons (car seq)
            (remove-last (cdr seq)))))

(define (safe? k positions)
  (define (check-row k)
    (not (in (at k positions) (remove-last positions))))
  (define (check-diagno op row col)
    (if (or (< row 0) (< col 0) (> row 7) (> col 7))
        #t
        (and (not (= (at col positions) row))
             (check-diagno op (op row 1) (- col 1)))))
  (and (check-row k)
       (check-diagno + (+ (at k positions) 1) (- k 1))
       (check-diagno - (- (at k positions) 1) (- k 1))))


;;2.46
;;vect
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
;;vect operation
(define (add-vect u v)
  (make-vect (+ (xcor-vect u) (xcor-vect v))
             (+ (ycor-vect u) (ycor-vect v))))

(define (sub-vect u v)
  (make-vect (- (xcor-vect u) (xcor-vect v))
             (- (ycor-vect u) (ycor-vect v))))

(define (scale-vect v s)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;;2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

;(define (make-frame origin edge1 edge2)
;  (cons origin (cons edge1 edge2)))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (car (cdr (cdr frame))))
;(define (edge2-frame frame)
;  (cdr (cdr frame)))

;;2.48
;(define (make-segment p q) (cons p q))
;(define (start-segment seg) (car seg))
;(define (end-segment seg) (cdr seg))

;;2.54
(define (equal? a b)
  (if (and (null? a) (null? b))
      #t
  (let (
        (ca (car a))
        (cb (car b))
        )
    (if (and (pair? ca) (pair? cb))
        (and (equal? ca cb)
             (equal? (cdr a) (cdr b)))
        (and (eq? ca cb) 
             (equal? (cdr a) (cdr b)))
        ))))