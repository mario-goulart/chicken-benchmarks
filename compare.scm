#! /bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

(module compare ()

(import chicken scheme)
(use data-structures extras files irregex posix srfi-13 srfi-1)

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


(define-record log file version repetitions installation-prefix csc-options results)


(define (read-log log-file)
  (let ((log-data (with-input-from-file log-file read)))
    (make-log log-file
              (alist-ref 'log-format-version log-data)
              (alist-ref 'repetitions log-data)
              (alist-ref 'installation-prefix log-data)
              (alist-ref 'csc-options log-data)
              (alist-ref 'results log-data))))

(define (average data)
  (/ (apply + data) (length data)))

(define (get-log-results-by-metric log metric)
  ;;1> (define l (with-input-from-file "benchmark.log" read))
  ;;2> (define r (alist-ref 'results l))
  (let ((results (log-results log)))
    (case metric
      ((build-time)
       (map (lambda (result)
              (cons (car result)
                    (cadr result)))
            results))
      ((bin-size)
       (map (lambda (result)
              (cons (car result)
                    (caddr result)))
            results))
      (else
       (map (lambda (prog-data)
              (let ((prog (car prog-data))
                    (result-subsets (cdddr prog-data)))
                (cons prog
                      (map (lambda (result-subset)
                             (alist-ref metric result-subset))
                           result-subsets))))
            results)))))


(define (compare logs metrics)
  (unless (every (lambda (v) (= v 2)) (map log-version logs))
    (fprintf (current-error-port)
             "Only log version 2 is supported.\n")
    (for-each (lambda (log)
                (fprintf (current-error-port)
                         "  * ~a: version ~a\n"
                         (log-file log)
                         (log-version log)))
              logs)
    (exit 1))
  (let ((progs (sort (map car (log-results (car logs))) string<)))
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
                    ;; If a test is missing then prog-results is #f
                    (if (or (memq metric '(build-time bin-size))
                            (not prog-results))
                        prog-results
                        (average prog-results))))
                logs)))
        progs)
       (print "\n"))
     metrics)))

(define (parse-metrics-from-command-line args all-metrics)
  (or (and-let* ((m (cmd-line-arg '--metrics args))
                 (ms (map string->symbol
                          (string-split m ","))))
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
        ms)
      '(cpu-time)))

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
Usage:
   #program --list-metrics
   #program [ --metrics=m1[,m2,...] ] log-file-1 log-file-2 ...

If metrics are not specified, only results for cpu-time will be displayed.

EOF
    port)
    (when exit-code (exit exit-code))))

(let* ((args (command-line-arguments))
       (non-option-args
        (remove (lambda (arg)
                  (string-prefix? "--" arg))
                args))
       (all-metrics '(build-time bin-size cpu-time major-gcs-time mutations
                      mutations-tracked major-gcs minor-gcs)))

  (when (member "--list-metrics" args)
    (for-each print all-metrics)
    (exit 0))

  (when (null? non-option-args)
    (usage 1))

  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (let ((metrics (parse-metrics-from-command-line args all-metrics)))
    (compare (map read-log non-option-args) metrics)))

) ;; end module
