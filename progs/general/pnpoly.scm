;;; PNPOLY - Test if a point is contained in a 2D polygon.

(cond-expand
 ((or chicken-5 chicken-6)
  (import (chicken fixnum)))
 (else))

(define (pt-in-poly2 xp yp x y)
  (let loop ((c #f) (i (- (vector-length xp) 1)) (j 0))
    (if (< i 0)
      c
      (if (or (and (or (> (vector-ref yp i) y)
                       (>= y (vector-ref yp j)))
                   (or (> (vector-ref yp j) y)
                       (>= y (vector-ref yp i))))
              (>= x
                       (+ (vector-ref xp i)
                               (/ (*
                                        (- (vector-ref xp j)
                                                (vector-ref xp i))
                                        (- y (vector-ref yp i)))
                                       (- (vector-ref yp j)
                                               (vector-ref yp i))))))
        (loop c (- i 1) i)
        (loop (not c) (- i 1) i)))))

(define (run input1 input2)
  (let ((count 0)
        (xp (list->vector (vector->list input1)))
        (yp (list->vector (vector->list input2))))
    (if (pt-in-poly2 xp yp .5 .5) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp .5 1.5) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp -.5 1.5) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp .75 2.25) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp 0. 2.01) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp -.5 2.5) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp -1. -.5) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp -1.5 .5) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp -2.25 -1.) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp .5 -.25) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp .5 -1.25) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp -.5 -2.5) (set! count (+ count 1)))
    count))

(time
 (let loop ((i 50000))
   (unless (zero? i)
     (run 
      '#(0. 1. 1. 0. 0. 1. -.5 -1. -1. -2. -2.5 -2. -1.5 -.5 1. 1. 0. -.5 -1. -.5)
      '#(0. 0. 1. 1. 2. 3. 2. 3. 0. -.5 -1. -1.5 -2. -2. -1.5 -1. -.5 -1. -1. -.5))
     (loop (fx- i 1)))))
