#!/bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

(import chicken scheme)
(use data-structures extras files)

(define (convert-1->2 log-file)
  (let* ((data (with-input-from-file log-file read))
         (version (alist-ref 'log-format-version data)))
    (when (or (not version)
              (not (= version 1)))
      (fprintf (current-error-port)
               "Unsupported log version: ~a.  Only log version 1 is supported.\n"
               version)
      (exit 1))
    (pp
     (let loop ((old-log data))
       (if (null? old-log)
           '()
           (let ((form (car old-log)))
             (case (car form)
               ((results)
                (let ((results
                       (let handle-results ((old-results (cdr form)))
                         (if (null? old-results)
                             '()
                             (let ((result (car old-results))) ;; result block
                               (cons
                                (append
                                 (list (car result)   ;; program
                                       (cadr result)  ;; build-time
                                       0)             ;; bin-size
                                 (cddr result))       ;; rest
                                (handle-results (cdr old-results))))))))
                  (cons (cons 'results results) (loop (cdr old-log)))))
               ((log-format-version)
                (cons '(log-format-version . 2) (loop (cdr old-log))))
               (else
                (cons form (loop (cdr old-log)))))))))))

(define (usage #!optional exit-code)
  (let ((program (pathname-strip-directory (program-name)))
        (port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port))))
    (display #<#EOF
Usage: #program <log-file>

Convert the given <log-file>'s format from version 1 to version 2,
filling bin-size with 0.  The new log file will be printed to the
standard output.

EOF
    port)
    (when exit-code (exit exit-code))))


(let ((args (command-line-arguments)))

  (when (null? args)
    (usage 1))

    ;; Check help options
  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (convert-1->2 (car args)))
