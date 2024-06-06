#! /bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

(module query-benchmark-log ()

(import scheme)
(cond-expand
  (chicken-4
   (import chicken)
   (use data-structures files)

   (begin-for-syntax (require-library files))
   (define-syntax include-relative
     (ir-macro-transformer
      (lambda (exp inject comp)
        (let ((path (cadr exp)))
          `(include
            ,(make-pathname (pathname-directory ##sys#current-load-path)
                            path))))))
   )
  ((or chicken-5 chicken-6)
   (import (chicken base)
           (chicken pathname)
           (chicken process-context)))
  (else
   (error "Unsupported CHICKEN version.")))

(include-relative "./lib/utils.scm")

(define (read-log log-file)
  (with-input-from-file log-file read))

(define (query-csc-options log-file)
  (alist-ref 'csc-options (read-log log-file)))

(define (query-programs log-file)
  (let* ((results (alist-ref 'results (read-log log-file))))
    (map (lambda (prog-data)
           (alist-ref 'program prog-data))
         results)))

(define (query-repetitions log-file)
  (alist-ref 'repetitions (read-log log-file)))

(define (query-bench-data field log-file programs)
  (let ((all-results (alist-ref 'results (read-log log-file))))
    (let loop ((times '())
               (progs-data all-results))
      (if (null? progs-data)
          times
          (let* ((prog-data (car progs-data))
                 (data-blocks (alist-ref 'results prog-data)))
            (loop
             (if (or (not programs)
                     (member (alist-ref 'program prog-data) programs))
                 (append
                  (map (lambda (block)
                         (alist-ref field block))
                       data-blocks)
                  times)
                 times)
             (cdr progs-data)))))))

(define (sum-up-field-values field log-file programs)
  (let ((results (query-bench-data field log-file programs)))
    (if (any not results)
        (die! "FAIL")
        (apply + results))))

(define (query-runtime-options log-file)
  (alist-ref 'runtime-options (read-log log-file)))

(define (find-program-data program results)
  (let loop ((results results))
    (if (null? results)
        #f
        (let ((program-data (car results)))
          (if (equal? program (alist-ref 'program program-data))
              program-data
              (loop (cdr results)))))))

(define (query-build-time log-file programs)
  ;; If program is #f, query all programs
  (let ((results (alist-ref 'results (read-log log-file))))
    (let loop ((programs (or programs (map (lambda (prog-data)
                                             (alist-ref 'program prog-data))
                                           results))))
      (if (null? programs)
          0
          (let* ((program (car programs))
                 (program-data (find-program-data program results)))
            (unless program-data
              (die! "Error: could not find benchmark data for program ~a."
                    program))
            (+ (alist-ref 'build-time program-data)
               (loop (cdr programs))))))))

(define (usage #!optional exit-code)
  (let ((program (pathname-strip-directory (program-name)))
        (port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port))))
    (display #<#EOF
Usage: #program <command> [<options>] log-file

<command>s are:
csc-options
programs
repetitions
runtime-options
build-time [--programs=<prog1>[,prog2...]]
cpu-time [--programs=<prog1>[,prog2...]]
major-gcs-time [--programs=<prog1>[,prog2...]]
mutations [--programs=<prog1>[,prog2...]]
mutations-tracked [--programs=<prog1>[,prog2...]]
major-gcs [--programs=<prog1>[,prog2...]]
minor-gcs [--programs=<prog1>[,prog2...]]

EOF
    port)
    (when exit-code (exit exit-code))))


(let* ((parsed-args (parse-cmd-line (command-line-arguments)
                                    '((--programs))))
       (args (cdr parsed-args))
       (nonamed-args (car parsed-args)))

  (when (help-requested? args)
    (usage 0))

  (when (or (null? nonamed-args)
            (null? (cdr nonamed-args)))
    (usage 1))

  (let* ((command (string->symbol (car nonamed-args)))
         (log-file (cadr nonamed-args))
         (output
          (case command
            ((csc-options)
             (query-csc-options log-file))
            ((programs)
             (query-programs log-file))
            ((repetitions)
             (query-repetitions log-file))
            ((build-time cpu-time major-gcs-time mutations mutations-tracked
                         major-gcs minor-gcs)
             (let* ((programs-str (cmd-line-arg '--programs args))
                    (programs (and programs-str
                                   (string-split programs-str ","))))
               (if (eq? command 'build-time)
                   (query-build-time log-file programs)
                   (sum-up-field-values command log-file programs))))
            ((runtime-options)
             (query-runtime-options log-file))
            (else
             (die! "~a: invalid command." command)))))
    (if (list? output)
        (for-each print output)
        (print output))))

) ; end module
