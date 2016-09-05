;;; SUM1 -- One of the Kernighan and Van Wyk benchmarks.
;;; http://cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html

(define (sumport port sum-so-far)
  (let ((x (read port)))
    (if (eof-object? x)
        sum-so-far
        (sumport port (+ x sum-so-far)))))

(define (sum port)
  (sumport port 0.0))

(define (go input)
  (call-with-input-file input sum))

(time (go "fp-numbers.data"))
