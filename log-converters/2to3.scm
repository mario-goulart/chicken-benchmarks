#!/bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

(import scheme)
(cond-expand
  (chicken-4
   (import chicken))
  ((or chicken-5 chicken-6)
   (import (chicken pretty-print)
           (chicken process-context)))
  (else
   (error "Unsupported CHICKEN version.")))


(define (convert-2->3 log-file)
  ;; Append max-live-heap to results and deviances
  (let* ((log-data (with-input-from-file log-file read))
         (results (alist-ref 'results log-data)))
    (pp
     (list
      (cons 'log-format-version 3)
      (assq 'repetitions log-data)
      (assq 'installation-prefix log-data)
      (assq 'csc-options log-data)
      (assq 'runtime-options log-data)
      (cons 'results
            (map (lambda (result)
                   (list
                    (assq 'program result)
                    (assq 'build-time result)
                    (cons 'deviances
                          (append (alist-ref 'deviances result)
                                  (list (cons 'max-live-heap 0))))
                    (cons 'results
                          (map (lambda (result)
                                 (append result
                                         (list (cons 'max-live-heap 0))))
                               (alist-ref 'results result)))))
                 results))))))


(define (usage #!optional exit-code)
  (let ((program (pathname-strip-directory (program-name)))
        (port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port))))
    (display #<#EOF
Usage: #program <log-file>

<log-file> is a log file whose format version is 2.  A log file of format 3
will be printed to stdout.

EOF
)
    (when exit-code (exit exit-code))))


(let ((args (command-line-arguments)))
  (when (null? args)
    (usage 1))

  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (convert-2->3 (car args)))
