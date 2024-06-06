;;; TAIL -- One of the Kernighan and Van Wyk benchmarks.
;;; http://cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html
;;;
;;; Modified for R7RS by Will Clinger.
;;;
;;; The key idea of this benchmark is that, for each iteration,
;;; the entire input is read line by line before any output
;;; is produced, and the lines are then written to the output
;;; in the reverse of the order in which they were read.
(cond-expand
 (chicken-4
  (use extras))
 ((or chicken-5 chicken-6)
  (import (chicken io) (chicken file))
  (cond-expand
   (chicken-6
    (import (rename (scheme base) (write-string %write-string)))
    (define (write-string str port start) (%write-string str start port))
    ))))

(define (tail-r-aux port file-so-far)
  (let ((x (read-line port)))
    (if (eof-object? x)
        file-so-far
        (tail-r-aux port (cons x file-so-far)))))

(define (echo-lines-in-reverse-order in out)
  (for-each (lambda (line) (write-string line #f out) (newline out))
            (tail-r-aux in '())))

(define (go input output)
  (call-with-input-file
   input
   (lambda (in)
     (if (file-exists? output) (delete-file output))
     (call-with-output-file
      output
      (lambda (out)
        (echo-lines-in-reverse-order in out))))))

(time (go "inputs/fp-numbers.data" "fp-numbers.data.out"))
