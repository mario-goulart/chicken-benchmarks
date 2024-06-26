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
  ((or chicken-5 chicken-6)
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

(define (apply-multiplier n)
  (cond ((> n (expt 1024 3))
         (sprintf "~a GiB" (truncate* (/ n (expt 1024 3)))))
        ((> n (expt 1024 2))
         (sprintf "~a MiB" (truncate* (/ n (expt 1024 2)))))
        ((> n 1024)
         (sprintf "~a KiB" (truncate* (/ n 1024))))
        (else
         (sprintf "~a bytes" n))))

(define metrics/units
  ;; Map metrics to units. A unit can be a string or a procedure that
  ;; will be given a value and must produce a string.
  `((build-time           . "s")
    (cpu-time             . "s")
    (major-gcs-time       . "s")
    (major-gcs            . "")
    (minor-gcs            . "")
    (mutations            . "")
    (mutations-tracked    . "")
    (max-live-heap        . ,apply-multiplier)))

(define (apply-unit val metric)
  (if val
      (let ((unit (alist-ref metric metrics/units)))
        (if (procedure? unit)
            (unit val)
            (sprintf "~a~a" (truncate* val) unit)))
      "FAIL"))

(define (get-metrics log-format-version)
  (let ((all-metrics (map car metrics/units)))
    ;; max-live-heap was added in log format version 3
    (if (< log-format-version 3)
        (delete 'max-live-heap all-metrics)
        all-metrics)))

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
     (else (average (filter identity prog-results))))))

(define (get-program-deviance log prog metric)
  (let loop ((results (log-results log)))
    (if (null? results)
        '()
        (let* ((result (car results))
               (program (alist-ref 'program result)))
          (if (string=? prog program)
              (alist-ref metric (alist-ref 'deviances result))
              (loop (cdr results)))))))

(define (get-deviances-by-metric-program logs metrics progs)
  ;; Returns a list like
  ;;
  ;; ((<metric> ((<prog1> <deviance log1> ...) ...))
  ;;  ...)
  (map (lambda (metric)
         (cons metric
               (map (lambda (prog)
                      (cons prog
                            (map (lambda (log)
                                   (get-program-deviance log prog metric))
                                 logs)))
                    progs)))
       metrics))

(define (compare-text logs metrics max-deviance labels)
  ;; max-deviance is currently not used here (it's only here so that
  ;; compare-text has the same signature as compare-html.
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
              (printf "[~a]: ~a (~a)\n"
                      log-idx
                      (highlight-value (normalize-result time best worst) #f)
                      (apply-unit (truncate* time) metric))))
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

(define (compare-html logs metrics max-deviance labels)
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
                        " ")))
             (li (a (@ (href "#deviances-by-metric-percentages"))
                    "Deviances by metric (raw percentages)")
                 (ul ,@(map (lambda (metric)
                              `(li (a (@ (href ,(sprintf "#~a-deviances-percentages" metric)))
                                      ,metric)))
                            metrics))))

            (h2 (@ (id "logs")) "Logs")
            ,@(map (lambda (log log-idx label)
                     `((h3 (span (@ (style ,(sprintf "background-color: ~a"
                                                     (list-ref colors log-idx))))
                                 ,label))
                       (ul ,@(map (lambda (opt)
                                    `(li ,(car opt) ": " (code ,(cdr opt))))
                                  (list-ref bench-options log-idx)))))
                   logs
                   enumerated-logs
                   labels)

            (h2 (@ (id "overall-results")) "Overall benchmark results")
            (p "This is the sum of all results of all programs, classified by metric.")
            ,@(map (lambda (metric)
                     `((h3 (@ (id ,(sprintf "overall-results-~a" metric))) ,metric)
                       ,(plot-chart
                         (map (lambda (log label)
                                (list label
                                      (truncate* (get-overall-total-by-metric log metric))))
                              logs
                              labels)
                         unit-printer: (lambda (n) (apply-unit n metric)))))
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
                            (vals (apply append (map cdr progs/logs-vals)))
                            (max-val (apply max (filter identity vals))))
                       `((h3 (@ (id ,(sprintf "results-by-metric-~a" metric)))
                             ,metric)
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
                                    (map (lambda (log label)
                                           (list label
                                                 (get-results-by-metric-prog
                                                  log metric prog)))
                                         logs
                                         labels)
                                    unit-printer: (lambda (n)
                                                    (apply-unit n metric)))))
                              metrics)))
                   progs)

            (h2 (@ (id "deviances-by-metric-percentages"))
                "Deviances by metric (raw percentages)")
            (p "Deviances greater than " ,max-deviance "% are highlighted.")
            ,(let* (;; build-time is a one-shot measurement -- no deviance
                    (deviance-metrics (delete 'build-time metrics))
                    (deviances
                     (get-deviances-by-metric-program logs deviance-metrics progs)))
               (map (lambda (metric)
                      `((h3 (@ (id ,(sprintf "~a-deviances-percentages" metric)))
                            ,metric)
                        ,(let ((metric-deviances (alist-ref metric deviances)))
                           (zebra-table
                            (cons "Program" enumerated-logs)
                            (map (lambda (prog)
                                   (let ((prog-deviances
                                          (alist-ref prog metric-deviances equal?)))
                                     (cons prog
                                           (map (lambda (val)
                                                  (if val
                                                      (if (> val max-deviance)
                                                          (highlight-worst (truncate* val) #t)
                                                          (truncate* val))
                                                      "FAIL"))
                                                prog-deviances))))
                                 progs)))))
                    deviance-metrics))
            )))
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

(define (infer-labels log-files)
  (define (get-common-prefix paths)
    (if (any null? paths)
        '()
        (let ((first-path-components (map car paths))
              (maybe-common (caar paths)))
          (if (every (lambda (prefix)
                       (string=? prefix maybe-common))
                     first-path-components)
              (cons maybe-common
                    (get-common-prefix (map cdr paths)))
              '()))))
  (let ((common-prefix
         (get-common-prefix
          (map (lambda (path)
                 (string-split path "/"))
               log-files))))
    (map pathname-strip-extension
         (if (null? common-prefix)
             log-files
             (let ((len-common-prefix
                    (+ (length common-prefix)
                       (apply + (map string-length common-prefix)))))
               (map (lambda (path)
                      (substring path (sub1 len-common-prefix)))
                    log-files))))))

(define (usage #!optional exit-code)
  (let ((program (pathname-strip-directory (program-name)))
        (port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port))))
    (display #<#EOF
Usage:
   #program --list-metrics
   #program <options> log-file-1 log-file-2 ...

<options>:
--metrics=m1[,m2,...]
  Comma-separated list of metrics to compare.  If not specified, only results
  for cpu-time will be displayed.  Use --list-metrics to see all metrics.

--max-deviance=<percentage>
  If provided, results whose deviance are greater than <percentage> will be
  highlighted.  If omitted, 5% will be used.

--label=<label> ...
  Label to be used for chart bars in HTML mode.  If provided, the number of
  --label options must match the number of log files provided as inputs.
  If not provided, labels will be infered from the log file paths.

--html
  If provided, HTML code will be printed to the standard output.

EOF
    port)
    (when exit-code (exit exit-code))))

(let* ((parsed-args (parse-cmd-line (command-line-arguments)
                                    '(--html
                                      --help
                                      (--label)
                                      --list-metrics
                                      (--max-deviance)
                                      (--metrics))))
       (args (cdr parsed-args)))

  (when (help-requested? args)
    (usage 0))

  (let* ((log-files (car parsed-args))
         (html? (cmd-line-arg '--html args))
         (labels (cmd-line-arg '--label args multiple?: #t))
         (logs-data (map read-log log-files))
         (all-metrics (get-metrics (log-version (car logs-data)))))

    (when (cmd-line-arg '--list-metrics args)
      (for-each print (cons 'all all-metrics))
      (exit 0))

    (when (null? log-files)
      (usage 1))

    (when (and (not (null? labels))
               (not (= (length labels) (length log-files))))
      (die! "The number of --label parameters must be equal to the number of log files."))

    (let ((metrics (parse-metrics-from-command-line args all-metrics))
          (max-deviance (or (cmd-line-arg '--max-deviance args) "5"))
          (printer (if html? compare-html compare-text))
          (labels (if (null? labels)
                      (infer-labels log-files)
                      labels)))
      (check-logs! logs-data)
      (printer logs-data
               metrics
               (string->number max-deviance)
               labels))))

) ;; end module
