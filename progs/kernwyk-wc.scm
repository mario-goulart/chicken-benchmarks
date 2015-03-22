;;; WC -- One of the Kernighan and Van Wyk benchmarks.
;;; Rewritten by Will Clinger into more idiomatic (and correct!) Scheme.

(define (wcport port)
  (define (loop nl nw nc inword?)
    (let ((x (read-char port)))
      (cond ((eof-object? x)
             (list nl nw nc))
            ((char=? x #\space)
             (loop nl nw (+ nc 1) #f))
            ((char=? x #\newline)
             (loop (+ nl 1) nw (+ nc 1) #f))
            (else
             (loop nl (if inword? nw (+ nw 1)) (+ nc 1) #t)))))
  (loop 0 0 0 #f))

(define (go x)
  (call-with-input-file x wcport))
 
(time (go "fp-numbers.data"))
