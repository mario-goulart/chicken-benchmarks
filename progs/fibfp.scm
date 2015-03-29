;;; FIBFP -- Computes fib(35) using floating point

(define (fibfp n)
  (if (< n 2.)
    n
    (+ (fibfp (- n 1.))
       (fibfp (- n 2.)))))

(time (fibfp 30.0))
