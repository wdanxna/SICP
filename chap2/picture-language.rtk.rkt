#lang racket/gui
(require graphics/graphics)
(open-graphics)
(define vp-width 500)
(define vp-height 500)
(define vp (open-viewport "A Picture Language" vp-width vp-height))
(define draw (draw-viewport vp))
(define clear (clear-viewport vp))
(define line (draw-line vp))



(define (for-each proc seq)
  (if (null? (cdr seq))
      (proc (car seq))
      ((lambda (x)
         (proc (car seq))
         (for-each proc (cdr seq))) 0)))
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

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;;2.47
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (cdr (cdr frame)))

;;2.48
(define (make-segment p q) (cons p q))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))
(define (midpoint-segment seg)
  (scale-vect 0.5
              (add-vect (start-segment seg) (end-segment seg))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (segment->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (let ((start-v ((frame-coord-map frame) (start-segment segment)))
             (end-v ((frame-coord-map frame) (end-segment segment))))
         (line
          (make-posn (xcor-vect start-v) (- vp-height (ycor-vect start-v)))
          (make-posn (xcor-vect end-v) (- vp-height (ycor-vect end-v))))))
     segment-list)))

;2.49
;;a
(define outline-painter
  (segment->painter (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0))
                         (make-segment (make-vect 1.0 0.0) (make-vect 1.0 1.0))
                         (make-segment (make-vect 1.0 1.0) (make-vect 0.0 1.0))
                         (make-segment (make-vect 0.0 1.0) (make-vect 0.0 0.0)))))

;;b
(define x-painter
  (segment->painter (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
                          (make-segment (make-vect 0.0 1.0) (make-vect 1.0 0.0)))))

;;c
(define diamond-painter
  (let ((top (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0)))
        (right (make-segment (make-vect 1.0 0.0) (make-vect 1.0 1.0)))
        (bottom (make-segment (make-vect 1.0 1.0) (make-vect 0.0 1.0)))
        (left (make-segment (make-vect 0.0 1.0) (make-vect 0.0 0.0))))
    (segment->painter (list (make-segment (midpoint-segment top) (midpoint-segment right))
                            (make-segment (midpoint-segment right) (midpoint-segment bottom))
                            (make-segment (midpoint-segment bottom) (midpoint-segment left))
                            (make-segment (midpoint-segment left) (midpoint-segment top))))))

;;d
(define wave-painter
  (segment->painter
   (list
    (make-segment (make-vect 0.5 0.4) ;;; leg triangle
                  (make-vect 0.6 0))
    (make-segment (make-vect 0.5 0.4)
                  (make-vect 0.4 0))
    (make-segment (make-vect 0.3 0)
                  (make-vect 0.35 0.4))
    (make-segment (make-vect 0.35 0.4)
                  (make-vect 0.3 0.7))
    (make-segment (make-vect 0.3 0.7)
                  (make-vect 0.2 0.6))
    (make-segment (make-vect 0.2 0.6)
                  (make-vect 0 0.8))
    (make-segment (make-vect 0 0.9)
                  (make-vect 0.2 0.7))
    (make-segment (make-vect 0.2 0.7)
                  (make-vect 0.3 0.75))
    (make-segment (make-vect 0.3 0.75)
                  (make-vect 0.4 0.75))
    (make-segment (make-vect 0.4 0.75)
                  (make-vect 0.35 0.9))
    (make-segment (make-vect 0.35 0.9)
                  (make-vect 0.4 1))
    (make-segment (make-vect 0.5 1)
                  (make-vect 0.55 0.9))
    (make-segment (make-vect 0.55 0.9)
                  (make-vect 0.5 0.75))
    (make-segment (make-vect 0.5 0.75)
                  (make-vect 0.6 0.75))
    (make-segment (make-vect 0.6 0.75)
                  (make-vect 1 0.45))
    (make-segment (make-vect 1 0.3)
                  (make-vect 0.6 0.5))
    (make-segment (make-vect 0.6 0.5)
                  (make-vect 0.7 0)))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter 
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))


(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0)
                              (make-vect 0.5 1.0))))
    (lambda (frame)
          (paint-left frame)
          (paint-right frame)))))

;;2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (rotate90 (rotate180 painter)))

(define F-painter
  (segment->painter (list
                     (make-segment (make-vect 0.25 0.0) (make-vect 0.25 0.8))
                     (make-segment (make-vect 0.25 0.8) (make-vect 0.8 0.8))
                     (make-segment (make-vect 0.25 0.5) (make-vect 0.8 0.5)))))


;;2.51
(define (below painter1 painter2)
    (let ((up-painter (transform-painter painter2
                                         (make-vect 0.0 0.5)
                                         (make-vect 1.0 0.5)
                                         (make-vect 0.0 1.0)))
          (bottom-painter (transform-painter painter1
                                             (make-vect 0.0 0.0)
                                             (make-vect 1.0 0.0)
                                             (make-vect 0.0 0.5))))
      (lambda (frame)
      (up-painter frame)
      (bottom-painter frame))))

(define (below2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))


(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up-smaller (beside (beside (up-split painter (- n 1))
                                        (up-split painter (- n 1)))
                                (corner-split painter (- n 1))))
            (right-smaller (beside painter (below (right-split painter (- n 1))
                                                  (right-split painter (- n 1))))))
        (below right-smaller up-smaller))))

(define (square-limit painter n)
  (if (= n 0)
      painter
      (let ((up (beside (flip-horiz (square-limit painter (- n 1)))
                        (square-limit painter (- n 1)))))
        (below (flip-vert up) up))))

(define frame (make-frame (make-vect 0 0) (make-vect 500 0) (make-vect 0 500)))
