#lang sicp

(define (map proc . lists)
  (define (map-helper proc list)
    (if (null? list)
        '()
        (cons (proc (car list))
              (map-helper proc (cdr list)))))

  (cond ((null? (car lists)) '())
        (else (cons (apply proc (map-helper car lists))
                    (apply map (cons proc (map-helper cdr lists)))))))

(define (delay exp)
  (lambda () exp))
