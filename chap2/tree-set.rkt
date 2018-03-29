#lang planet neil/sicp
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        (else (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set)
         (make-tree x '() '()))
        ((< x (entry set))
         (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set) (adjoin-set x (right-branch set))))
        (else set)))



(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size))
              (right-size (- n (+ left-size 1))))
          (let ((non-left-elts (cdr left-result))
                (this-entry (cadr left-result)))
            (let ((right-result
                   (partial-tree (cdr non-left-elts) right-size)))
              (let ((non-right-elts (cdr right-result)))
                (cons
                 (make-tree this-entry
                            (car left-result)
                            (car right-result))
                 non-right-elts))))))))


;;2.65
(define (intersection-set-ordered-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (cond ((= (car set1) (car set2))
             (cons (car set1)
                   (intersection-set (cdr set1) (cdr set2))))
            ((< (car set1) (car set2))
             (intersection-set (cdr set1) set2))
            (else (intersection-set set1 (cdr set2))))))

(define (union-set-ordered-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2))
         (cons (car set1)(union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (cons (car set2)
                    (union-set set1 (cdr set2))))))

(define (union-set set1 set2)
  (list->tree (union-set-ordered-set (tree->list-1 set1) (tree->list-1 set2))))

(define (intersection-set set1 set2)
  (list->tree (intersection-set-ordered-set (tree->list-1 set1) (tree->list-1 set2))))

;;2.66
(define (key entry)
  (car entry))

(define (lookup key-id records)
  (if (null? records)
      false
      (cond ((equal? key-id (key (car records)))
             (car records))
            ((< key-id (key (car records)))
             (lookup key-id (left-branch records)))
            (else
             (lookup key-id (right-branch records))))))