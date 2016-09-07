;;; CAT -- One of the Kernighan and Van Wyk benchmarks.
;;; http://cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html
;;; Rewritten by Will Clinger into more idiomatic Scheme.

(define (catport in out)
  (let ((x (read-char in)))
    (if (not (eof-object? x))
        (begin
         (write-char x out)
         (catport in out)))))

(define (go input-file output-file)
  (if (file-exists? output-file)
      (delete-file output-file))
  (call-with-input-file
   input-file
   (lambda (in)
     (call-with-output-file
      output-file
      (lambda (out)
        (catport in out))))))

(time (go "inputs/fp-numbers.data" "fp-numbers.data.out"))
