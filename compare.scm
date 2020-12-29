#! /bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

(module compare ()

(import (except scheme log))
(cond-expand
  (chicken-4
   (import chicken)
   (use data-structures extras files posix)

   (begin-for-syntax (require-library files))
   (define-syntax include-relative
     (ir-macro-transformer
      (lambda (exp inject comp)
        (let ((path (cadr exp)))
          `(include
            ,(make-pathname (pathname-directory ##sys#current-load-path)
                            path))))))
   )
  (chicken-5
   (import (chicken base)
           (chicken pathname)
           (chicken process-context)
           (chicken sort)
           (only (chicken port) terminal-port?)))
  (else
   (error "Unsupported CHICKEN version.")))

(include-relative "./lib/utils.scm")

(define progs/pad 20)
(define results/pad 10)


(define (display-header logs)

  (define (show-option option value)
    (print "|-> " option ": " value))

  (let loop ((logs logs)
             (id 1))
    (unless (null? logs)
      (let ((log (car logs)))
        (print "+---[" id "]:")
        (show-option 'installation-prefix
                     (or (log-installation-prefix log)
                         "from $PATH"))
        (show-option 'csc-options (log-csc-options log))
        (show-option 'runtime-options (log-runtime-options log))
        (show-option 'repetitions (log-repetitions log))
        (newline))
      (loop (cdr logs) (+ id 1))))
  (print "Displaying normalized results (larger numbers indicate better results)\n"))

(define (display-columns-header logs)
  (let ((num-logs (length logs)))
    (display (string-pad-right "Programs" progs/pad #\space))
    (for-each
     (lambda (id)
       (display (string-pad
                 (string-append "[" (number->string id) "]")
                 results/pad
                 #\space)))
     (iota num-logs 1))
    (newline)
    (print (make-string (+ progs/pad (* num-logs results/pad)) #\=))))


(define ansi-term? ;; adapted from the test egg
  (and (terminal-port? (current-output-port))
       (member (get-environment-variable "TERM")
               '("xterm" "xterm-color" "xterm-256color" "rxvt" "kterm"
                 "linux" "screen" "screen-256color" "vt100"
                 "rxvt-unicode-256color"))))

(define (green text)
  (if ansi-term?
      (conc "\x1b[32m" text "\x1b[0m")
      text))


(define (red text)
  (if ansi-term?
      (conc "\x1b[31m" text "\x1b[0m")
      text))


(define (find-worst times)
  (let ((times (filter identity times)))
    (if (null? times)
        'FAIL
        (apply max times))))


(define (find-best times)
  (let ((times (filter identity times)))
    (if (null? times)
        'FAIL
        (apply min times))))


(define (fmt num)
  ;; formats num with two digits at the right of .
  (let* ((str-num (number->string num))
         (tokens (string-split str-num "."))
         (l (car tokens))
         (r (if (null? (cdr tokens))
                "00"
                (cadr tokens))))
    (string-append l "." (if (< (string-length r) 2)
                             (string-append r "0")
                             (string-take r 2)))))


(define (normalize worst time)
  (if (or (zero? worst)
          (zero? time))
      1
      (/ worst time)))

(define (display-results prog times)
  (display (string-pad-right prog 20 #\_))
  (let ((best (find-best times))
        (worst (find-worst times)))
    (for-each
     (lambda (time)
       (display (string-pad
                 (cond ((not time)
                        "FAIL")
                       ((= time best)
                        (green (fmt (normalize worst time))))
                       ((= time worst)
                        (red "1.00"))
                       (else (fmt (normalize worst time))))
                 (if (and ansi-term?
                          time
                          (or (= time best) (= time worst)))
                     (+ results/pad 9) ;; + ansi format chars
                     results/pad)
                 #\_)))
     times)
    (newline)))


(define-record log version repetitions installation-prefix csc-options runtime-options results)


(define (read-log log-file)
  (let ((log-data (with-input-from-file log-file read)))
    (make-log (alist-ref 'log-format-version log-data)
              (alist-ref 'repetitions log-data)
              (alist-ref 'installation-prefix log-data)
              (alist-ref 'csc-options log-data)
              (alist-ref 'runtime-options log-data)
              (alist-ref 'results log-data))))

(define (average data)
  (/ (apply + data) (length data)))

(define (get-log-results-by-metric log metric)
  (let ((results (log-results log)))
    (if (eq? metric 'build-time)
        (map (lambda (prog-data)
               (cons (alist-ref 'program prog-data)
                     (alist-ref 'build-time prog-data)))
             results)
        (map (lambda (prog-data)
               (let ((prog (alist-ref 'program prog-data))
                     (result-subsets (alist-ref 'results prog-data)))
                 (cons prog
                       (map (lambda (result-subset)
                              (alist-ref metric result-subset))
                            result-subsets))))
             results))))


(define (compare logs metrics)
  (let ((progs (sort (map (lambda (prog-data)
                            (alist-ref 'program prog-data))
                          (log-results (car logs)))
                     string<)))
    (display-header logs)
    (for-each
     (lambda (metric)
       (printf "===\n=== ~a\n===\n\n" metric)
       (display-columns-header logs)
       (for-each
        (lambda (prog)
          (display-results
           prog
           (map (lambda (log)
                  (let* ((results (get-log-results-by-metric log metric))
                         (prog-results (alist-ref prog results equal?)))
                    (cond
                     ;; If a test is missing then prog-results is #f
                     ((or (eq? metric 'build-time) (not prog-results))
                      prog-results)
                     ;; At least one execution failed
                     ((any not prog-results)
                      #f)
                     ;; Everything is ok.  Calculate the average
                     (else (average prog-results)))))
                logs)))
        progs)
       (print "\n"))
     metrics)))

(define (parse-metrics-from-command-line args all-metrics)
  (or (and-let* ((m (cmd-line-arg '--metrics args))
                 (ms (map string->symbol
                          (string-split m ","))))
        (if (memq 'all ms)
            ;; "all" is a special shortcut for all metrics
            all-metrics
            (begin
              (unless (every (lambda (metric)
                               (memq metric all-metrics))
                             ms)
                (fprintf (current-error-port)
                         "Invalid metrics: ~a.  Aborting.\n"
                         (string-intersperse
                          (map symbol->string
                               (filter (lambda (m)
                                         (not (memq m all-metrics)))
                                       ms))
                          ", "))
                (exit 1))
              ms)))
      '(cpu-time)))

(define (usage #!optional exit-code)
  (let ((program (pathname-strip-directory (program-name)))
        (port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port))))
    (display #<#EOF
Usage:
   #program --list-metrics
   #program [ --metrics=m1[,m2,...] ] log-file-1 log-file-2 ...

If metrics are not specified, only results for cpu-time will be displayed.

EOF
    port)
    (when exit-code (exit exit-code))))

(let* ((parsed-args (parse-cmd-line (command-line-arguments)
                                    '(--list-metrics
                                      (--metrics))))
       (args (cdr parsed-args))
       (log-files (car parsed-args))
       (all-metrics '(build-time cpu-time major-gcs-time mutations
                      mutations-tracked major-gcs minor-gcs)))

  (when (cmd-line-arg '--list-metrics args)
    (for-each print (cons 'all all-metrics))
    (exit 0))

  (when (help-requested? args)
    (usage 0))

  (when (null? log-files)
    (usage 1))

  (let ((metrics (parse-metrics-from-command-line args all-metrics)))
    (compare (map read-log log-files) metrics)))

) ;; end module
