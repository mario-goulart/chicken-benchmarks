;;; The Computer Language Benchmarks Game
;;; http://shootout.alioth.debian.org/

;;; based on Racket version by Matthew Flatt

;;; Adapted from chibi to CHICKEN --mario

(cond-expand
  (chicken-4
   (use data-structures extras srfi-69))
  ((or chicken-5 chicken-6)
   (import (chicken io) (chicken sort) srfi-69)
   (cond-expand
    (chicken-6
     (import (scheme base)))
    (else))))

(define (string-copy! dst dstart src start end)
  (do ((i dstart (+ i 1))
       (j start (+ j 1)))
      ((>= j end))
    (string-set! dst i (string-ref src j))))

(define (string-upcase str)
  (let* ((len (string-length str))
         (res (make-string len)))
    (do ((i 0 (+ i 1)))
        ((>= i len) res)
      (string-set! res i (char-upcase (string-ref str i))))))

(define (all-counts len dna)
  (let ((table (make-hash-table eq?))
        (seq (make-string len)))
    (do ((s (- (string-length dna) len) ( - s 1)))
        ((< s 0) table)
      (string-copy! seq 0 dna s (+ s len))
      (let ((key (string->symbol seq)))
        (let ((cnt (hash-table-ref/default table key 0)))
          (hash-table-set! table key (+ cnt 1)))))))

(define (write-freqs table)
  (let* ((content (hash-table->alist table))
         (total (exact->inexact (apply + (map cdr content)))))
    (for-each
     (lambda (a)
       (print (car a) " "
              (/ (round (* 100000.0 (/ (cdr a) total))) 1000.0)))
     (sort content
           (lambda (a b)
             (> (cdr a)
                (cdr b)))))))

(define (write-one-freq table key)
  (print (hash-table-ref/default table key 0) "\t" key))

(define dna
  (let ((in (open-input-file "inputs/knucleotide-input.txt")))
    ;; Skip to ">THREE ..."
    (let lp ()
      (let ((line (read-line in)))
        (cond ((eof-object? line))
              ((and (>= (string-length line) 6)
                    (eqv? #\> (string-ref line 0))
                    (equal? (substring line 0 6) ">THREE")))
              (else (lp)))))
    (let ((out (open-output-string)))
      ;; Copy everything but newlines to out:
      (let lp ()
        (let ((line (read-line in)))
          (cond ((eof-object? line))
                (else
                 (display line out)
                 (lp)))))
      ;; Extract the string from out:
      (string-upcase (get-output-string out)))))

(time
 (begin
   ;; 1-nucleotide counts:
   (write-freqs (all-counts 1 dna))
   (newline)

   ;; 2-nucleotide counts:
   (write-freqs (all-counts 2 dna))
   (newline)

   ;; Specific sequences:
   (for-each
    (lambda (seq)
      (write-one-freq (all-counts (string-length seq) dna)
                      (string->symbol seq)))
    '("GGT" "GGTA" "GGTATT" "GGTATTTTAATT" "GGTATTTTAATTTATAGT"))))
