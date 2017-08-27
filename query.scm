#! /bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

(module query-benchmark-log ()

(import chicken scheme)
(cond-expand
  (chicken-4
   (use data-structures extras irregex files ports posix srfi-1 srfi-13))
  (chicken-5
   (import (chicken pathname) (chicken data-structures) (chicken irregex)
           (chicken ports) (chicken format) srfi-1 srfi-13)))

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
        (begin
          (fprintf (current-error-port) "FAIL\n")
          (exit 1))
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
              (fprintf (current-error-port)
                       "Error: could not find benchmark data for program ~a.\n"
                       program)
              (exit 1))
            (+ (alist-ref 'build-time program-data)
               (loop (cdr programs))))))))

(define (cmd-line-arg option args)
  ;; Returns the argument associated to the command line option OPTION
  ;; in ARGS or #f if OPTION is not found in ARGS or doesn't have any
  ;; argument.
  (let ((val (any (lambda (arg)
                    (irregex-match
                     `(seq ,(->string option) "=" (submatch (* any)))
                     arg))
                  args)))
    (and val (irregex-match-substring val 1))))

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


(let ((args (command-line-arguments)))

  ;; Check help options
  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (let ((args-without-options
         (remove (lambda (arg)
                   (string-prefix? "--" arg))
                 args))
        (options (filter (lambda (arg)
                           (string-prefix? "--" arg))
                         args)))
    (when (or (null? args-without-options)
              (null? (cdr args-without-options)))
      (usage 1))
    (let* ((command (string->symbol (car args-without-options)))
           (log-file (cadr args-without-options))
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
               (let* ((programs-str (cmd-line-arg '--programs options))
                      (programs (and programs-str
                                     (string-split programs-str ","))))
                 (if (eq? command 'build-time)
                     (query-build-time log-file programs)
                     (sum-up-field-values command log-file programs))))
              ((runtime-options)
               (query-runtime-options log-file))
              (else
               (with-output-to-port (current-error-port)
                 (lambda ()
                   (printf "~a: invalid command.\n" command)
                   (exit 1)))))))
      (if (list? output)
          (for-each print output)
          (print output)))))

) ; end module
