#lang planet neil/sicp
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)(adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cdr pair))
                    (make-leaf-set (cdr pairs))))))

;;2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree 
                   (make-leaf 'B 2)
                   (make-code-tree 
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

;;2.68
(define (encode message tree)
  (if (null? message) 
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
;; mind the leaf situation
(define (encode-symbol symbol tree)
  (if (not (element-of-set? symbol (symbols tree)))
      (error "Invalid symbol to encode" symbol)
      (let ((left-b (left-branch tree))
            (right-b (right-branch tree)))
        (cond ((element-of-set? symbol (symbols left-b))
               (if (leaf? left-b)
                   '(0)
                   (cons 0
                         (encode-symbol symbol left-b))))
              (else 
               (if (leaf? right-b)
                   '(1)
                   (cons 1
                         (encode-symbol symbol right-b))))))))

;;because we make the symbols a unorderd list
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;;2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leafs)
  (if (= (length leafs) 1)
      (car leafs)
      (let ((merge (make-code-tree 
                    (car leafs)
                    (cadr leafs))))
        (successive-merge (adjoin-set merge (cdr (cdr leafs)))))))

;;2.70
(define lyrics-tree (generate-huffman-tree (list (cons 'A 2) 
                                                 (cons 'BOOM 1)
                                                 (cons 'GET 2)
                                                 (cons 'JOB 2)
                                                 (cons 'SHA 3)
                                                 (cons 'NA 16)
                                                 (cons 'WAH 1)
                                                 (cons 'YIP 9))))

(define lyrics '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(define lyrics-encoded (encode lyrics lyrics-tree))

(length lyrics-encoded);;86(huff) vs 108(fix-length)

;;2.71
;;n-2 bits for least frequent symbol, 1 for most frequent

;;2.72
;;n^2 for least frequent? 1 for most frequent
