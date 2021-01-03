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
(include-relative "./lib/html.scm")

(define progs/pad 20)
(define results/pad 10)

(define metrics/units
  '((build-time           . "s")
    (cpu-time             . "s")
    (major-gcs-time       . "s")
    (major-gcs            . "")
    (minor-gcs            . "")
    (mutations            . "")
    (mutations-tracked    . "")
    (major-gcs minor-gcs  . "")))

(define (get-log-bench-options logs)
  (map (lambda (log)
         `((installation-prefix
            . ,(or (log-installation-prefix log)
                   "from $PATH"))
           (csc-options . ,(log-csc-options log))
           (runtime-options . ,(log-runtime-options log))
           (repetitions . ,(log-repetitions log))))
       logs))

(define (display-header logs)
  (let ((bench-options (get-log-bench-options logs)))
    (let loop ((logs logs)
               (id 1))
      (unless (null? logs)
        (let ((log (car logs)))
          (print "+---[" id "]:")
          (for-each (lambda (opt)
                      (print "|-> " (car opt) ": " (cdr opt)))
                    (list-ref bench-options (sub1 id)))
          (newline))
        (loop (cdr logs) (+ id 1))))))

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

(define (green n)
  (if ansi-term?
      (conc "\x1b[32m" n "\x1b[0m")
      (number->string n)))

(define (red n)
  (if ansi-term?
      (conc "\x1b[31m" n "\x1b[0m")
      (number->string n)))

(define (highlight-best val html?)
  (if html?
      `(span (@ (class "best")) ,val)
      (green val)))

(define (highlight-worst val html?)
  (if html?
      `(span (@ (class "worst")) ,val)
      (red val)))

(define (highlight-value val html?)
  ;; val is a pair (<number|#f> . <best|worst|#f>).
  (let ((n (car val))
        (how-good (cdr val)))
    (if n
        (case how-good
          ((best) (highlight-best n html?))
          ((worst) (highlight-worst n html?))
          (else (number->string n)))
        "FAIL")))

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

(define (normalize worst time)
  (if (or (zero? worst)
          (zero? time))
      1.0
      (/ worst time 1.0)))

(define (normalize-result time best worst)
  (cond ((not time)
         (cons #f #f))
        ((= time best worst)
         (cons 1.0 #f))
        ((= time best)
         (cons (truncate* (normalize worst time)) 'best))
        ((= time worst)
         (cons 1.0 'worst))
        (else (cons (truncate* (normalize worst time)) #f))))

(define (display-results prog times)
  (display (string-pad-right prog 20 #\_))
  (let ((best (find-best times))
        (worst (find-worst times)))
    (for-each
     (lambda (time)
       (let* ((norm-res (normalize-result time best worst))
              (highlighted? (memq (cdr norm-res) '(best worst))))
         (display (string-pad
                   (highlight-value norm-res #f)
                   (if (and ansi-term? highlighted?)
                       (+ results/pad 9) ;; + ansi format chars
                       results/pad)
                 #\_))))
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

(define (get-overall-results-by-metric log metric)
  (if (eq? metric 'build-time)
      (map (lambda (prog-res)
             (alist-ref 'build-time prog-res))
           (log-results log))
      (let loop ((all-results (log-results log)))
        (if (null? all-results)
            '()
            (let ((prog-results (alist-ref 'results (car all-results))))
              (append (map (lambda (res)
                             (alist-ref metric res))
                           prog-results)
                      (loop (cdr all-results))))))))

(define (get-overall-total-by-metric log metric)
  (let ((results (filter identity (get-overall-results-by-metric log metric))))
    (apply + results)))

(define (get-results-by-metric-prog log metric prog)
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


(define (compare-text logs metrics)
  (let ((progs (sort (map (lambda (prog-data)
                            (alist-ref 'program prog-data))
                          (log-results (car logs)))
                     string<))
        (enumerated-logs (iota (length logs)))
        (overall-results
         (map (lambda (metric)
                (cons metric
                      (map (lambda (log)
                             (get-overall-total-by-metric log metric))
                           logs)))
              metrics)))

    (display-header logs)

    (print "===\n=== Overall results\n===")
    (for-each
     (lambda (metric)
       (let* ((metric-results (alist-ref metric overall-results))
              (best (find-best metric-results))
              (worst (find-worst metric-results)))
         (printf "\n=== [~a]\n" metric)
         (for-each
          (lambda (log log-idx)
            (let ((time (list-ref metric-results log-idx)))
              (printf "[~a]: ~a (~a~a)\n"
                      log-idx
                      (highlight-value (normalize-result time best worst) #f)
                      (truncate* time)
                      (alist-ref metric metrics/units))))
          logs
          enumerated-logs)))
     metrics)

    (print "\n\n===\n=== Results by metric\n===")

    (print "\n\nIn the tables below results are normalized "
           "(larger numbers indicate better results).\n")
    (for-each
     (lambda (metric)
       (printf "=== [~a]\n\n" metric)
       (display-columns-header logs)
       (for-each
        (lambda (prog)
          (display-results
           prog
           (map (lambda (log)
                  (get-results-by-metric-prog log metric prog))
                logs)))
        progs)
       (print "\n"))
     metrics)))

(define (compare-html logs metrics)
  (let* ((progs (sort (map (lambda (prog-data)
                             (alist-ref 'program prog-data))
                           (log-results (car logs)))
                      string<))
         (enumerated-logs (iota (length logs)))
         (bench-options (get-log-bench-options logs))
         (data
          `((h2 "Table of contents")
            (ul
             (li (a (@ (href "#logs")) "Logs"))
             (li (a (@ (href "#overall-results")) "Overall results"))
             (li (a (@ (href "#results-by-metric")) "Results by metric")
                 (ul ,@(map (lambda (metric)
                              `(li (a (@ (href ,(sprintf "#results-by-metric-~a" metric)))
                                      ,metric)))
                            metrics)))
             (li (a (@ (href "#results-by-program")) "Results by program")
                 (ul ,@(intersperse
                        (map (lambda (prog)
                               `(a (@ (href ,(sprintf "#results-by-program-~a" prog)))
                                   ,prog))
                             progs)
                        " "))))

            (h2 (@ (id "logs")) "Logs")
            ,@(map (lambda (log log-idx)
                     `((h3 (span (@ (class ,(sprintf "color-~a" log-idx)))
                                 "Log " ,log-idx))
                       (ul ,@(map (lambda (opt)
                                    `(li ,(car opt) ": " (code ,(cdr opt))))
                                  (list-ref bench-options log-idx)))))
                   logs
                   enumerated-logs)

            (h2 (@ (id "overall-results")) "Overall benchmark results")
            (p "This is the sum of all results of all programs, classified by metric.")
            ,@(map (lambda (metric)
                     `((h3 (@ (id ,(sprintf "overall-results-~a" metric))) ,metric)
                       ,(plot-chart
                         (map (lambda (log log-idx)
                                (list log-idx
                                      (truncate* (get-overall-total-by-metric log metric))))
                              logs
                              enumerated-logs)
                         unit: (alist-ref metric metrics/units))))
                   metrics)

            (h2 (@ (id "results-by-metric")) "Results by metric")
            (p "In the tables below results are normalized "
               "(larger numbers indicate better results).")
            ,@(map (lambda (metric)
                     (let* ((progs/logs-vals
                             (map (lambda (prog)
                                    (cons prog
                                          (map (lambda (log)
                                                 (get-results-by-metric-prog
                                                  log metric prog))
                                       logs)))
                                  progs))
                            (max-val
                             (apply max (apply append
                                               (map cdr progs/logs-vals)))))
                       `((h3 (@ (id ,(sprintf "results-by-metric-~a" metric))) ,metric)
                         ,(zebra-table
                           (cons "Program" enumerated-logs)
                           (map (lambda (prog)
                                  (let* ((prog-times
                                          (alist-ref prog progs/logs-vals equal?))
                                         (best (find-best prog-times))
                                         (worst (find-worst prog-times))
                                         (norm-prog-times
                                          (map (lambda (time)
                                                 (normalize-result time best worst))
                                               prog-times)))
                                    (cons prog
                                          (map (lambda (log-idx norm-val)
                                                 (let ((val (car norm-val)))
                                                   (if val
                                                       (highlight-value norm-val #t)
                                                       "FAIL")))
                                               enumerated-logs
                                               norm-prog-times))))
                                progs)))))
                   metrics)

            (h2 (@ (id "results-by-program")) "Results by program")
            ,@(map (lambda (prog)
                     `((h3 (@ (id ,(sprintf "results-by-program-~a" prog))) ,prog)
                       ,@(map (lambda (metric)
                                `((h4 ,metric)
                                  ,(plot-chart
                                    (map (lambda (log log-idx)
                                           (list log-idx
                                                 (get-results-by-metric-prog log metric prog)))
                                         logs
                                         enumerated-logs)
                                    unit: (alist-ref metric metrics/units))))
                              metrics)))
                   progs))
          ))
    (print (generate-html data))))


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

(define (check-logs! logs-data)
  ;; Verify that logs can be compared
  (let* ((log-format-versions (map log-version logs-data))
         (version (car log-format-versions)))
    (unless (every (lambda (v)
                     (= v version))
                   log-format-versions)
      (die! "Cannot compare logs of different format versions.")
      (exit 1))))

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
                                    '(--html
                                      --list-metrics
                                      (--metrics))))
       (args (cdr parsed-args))
       (log-files (car parsed-args))
       (html? (cmd-line-arg '--html args))
       (all-metrics (map car metrics/units)))

  (when (cmd-line-arg '--list-metrics args)
    (for-each print (cons 'all all-metrics))
    (exit 0))

  (when (help-requested? args)
    (usage 0))

  (when (null? log-files)
    (usage 1))

  (let ((metrics (parse-metrics-from-command-line args all-metrics))
        (printer (if html? compare-html compare-text))
        (logs-data (map read-log log-files)))
    (check-logs! logs-data)
    (printer logs-data metrics)))

) ;; end module
