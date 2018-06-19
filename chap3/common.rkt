#lang sicp
(#%provide
 gcd
 display-line
 map
 stream-car
 stream-cdr
 stream-ref
 display-stream
 print-stream
 stream-filter
 stream-map
 scale-stream
 add-streams
 mul-streams
 ones
 integers
 integral)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (display-line x)
  (display x)
  (newline))

;;generalised map
(define (map proc . lists)
  (define (map-helper proc lists)
    (if (null? lists)
        '()
        (cons (proc (car lists))
              (map-helper proc (cdr lists)))))
  (if (null? (car lists))
      '()
      (cons (apply proc (map-helper car lists))
            (apply map (cons proc (map-helper cdr lists))))))


;;stream package
(define (stream-car s)
  (car s))
(define (stream-cdr s)
  (force (cdr s)))

(define (stream-ref s i)
  (if (= i 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- i 1))))

;;打印整个流，适用于有穷流
(define (display-stream s)
  (if (not (stream-null? s))
      (begin
        (display-line (stream-car s))
        (display-stream (stream-cdr s)))))

;;测试过程,打印流的前n 项，适用与无穷流
(define (print-stream s n)
  (if (> n 0)
      (begin
        (display (stream-car s))
        (display " ")
        (print-stream (stream-cdr s) (- n 1)))))

(define (stream-filter pred stream)
  (if (stream-null? stream)
      the-empty-stream
      (if (pred (stream-car stream))
          (cons-stream (stream-car stream)
                       (stream-filter pred (stream-cdr stream)))
          (stream-filter pred (stream-cdr stream)))))

(define (stream-map proc . streams)
  (if (stream-null? (car streams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car streams))
                   (apply stream-map (cons proc (map stream-cdr streams))))))


(define (scale-stream s a)
  (stream-map (lambda (x) (* x a)) s))

;;element-wise add
(define (add-streams s1 s2)
  (stream-map + s1 s2))

;;element-wise multiply
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand (force delayed-integrand)))
       (add-streams (scale-stream integrand dt)
                    int))))
  int)


