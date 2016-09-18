#! /bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

;; This file is intentionally not wrapped into a module to ease the
;; use of a config file.  To run this file thorugh the scrutinizer,
;; use "csc -ASM run.scm".

(import chicken scheme)

(cond-expand
  (chicken-4
   (begin
     (use data-structures extras files posix utils srfi-1 srfi-13 irregex
          (only setup-api program-path))

     (define (read-full-string p) (read-all p))
     (define set-environment-variable! setenv)))

  (chicken-5
   (begin (import (chicken pretty-print) (chicken bitwise) (chicken format)
                  (chicken posix) (chicken data-structures) (chicken time)
                  (chicken pathname) (chicken io) (chicken irregex)
                  (only srfi-1 make-list last remove any iota)
                  (only srfi-13 string-trim-both string-pad-right)
                  (only setup-api program-path))

          (define (read-full-string p) (read-string #f p)) )))

;;; Configurable parameters
(define repetitions (make-parameter 10))
(define installation-prefix (make-parameter (pathname-directory (program-path))))
(define csc-options (make-parameter ""))
(define runtime-options (make-parameter ""))
(define log-file (make-parameter "benchmark.log"))
(define debug-file (make-parameter #f))
(define programs (make-parameter #f)) ;; list of symbols or #f (all programs)
(define skip-programs (make-parameter '())) ;; list of symbols
(define programs-dir (make-parameter (make-pathname "progs" "general")))

(define (all-progs)
  (map string->symbol
       (sort (map pathname-file
                  (glob (make-pathname (programs-dir) "*.scm")))
             string<?)))


(define (csc)
  (make-pathname (and (installation-prefix)
                      (list (installation-prefix) "bin"))
                 "csc"))


(define (run-shell-command command)
  ;; Returns (values <status> <output>)
  (let* ((start (current-milliseconds))
         (p (open-input-pipe (string-append command " 2>&1")))
         (output (read-full-string p))
         (duration (- (current-milliseconds) start))
         (exit-code (arithmetic-shift (close-input-pipe p) -8)))
    (when (debug-file)
      (with-output-to-file (debug-file)
        (lambda ()
          (print "Running '" command "'")
          (print "\tExit code = " exit-code)
          (print "\tOutput = " output))
        append:))
    (values exit-code
            output
            (/ duration 1000.0))))


(define-record bench-result
  cpu-time
  mutations
  mutations-tracked
  major-gcs-time
  major-gcs
  minor-gcs)

(define-record-printer (bench-result obj out)
  (fprintf
   out
   "<bench-result cpu-time: ~S, major-gcs-time: ~S, mutations: ~S, mutations-tracked: ~S, major-gcs: ~S, minor-gcs: ~S"
   (bench-result-cpu-time obj)
   (bench-result-major-gcs-time obj)
   (bench-result-mutations obj)
   (bench-result-mutations-tracked obj)
   (bench-result-major-gcs obj)
   (bench-result-minor-gcs obj)))

(define (bench-result->alist bench-result)
  `((cpu-time          . ,(bench-result-cpu-time bench-result))
    (major-gcs-time    . ,(bench-result-major-gcs-time bench-result))
    (mutations         . ,(bench-result-mutations bench-result))
    (mutations-tracked . ,(bench-result-mutations-tracked bench-result))
    (major-gcs         . ,(bench-result-major-gcs bench-result))
    (minor-gcs         . ,(bench-result-minor-gcs bench-result))))


(define parse-time-output
  (let* ((flonum '(or (+ num) (: (+ num) "." (+ num))))
         (cpu-time-pattern
          `(: (=> cpu-time ,flonum) "s CPU time"))
         (mutations-pattern
          '(or (: (=> mutations (+ num)) " mutations")
               (: (=> mutations (+ num)) "/" (=> mutations-tracked (+ num))
                  " mutations (total/tracked)")))
         (major-gcs-time-pattern
          `(: (=> major-gcs-time ,flonum) "s GC time (major)"))
         (gcs-pattern
          '(: (=> major-gcs (+ num)) "/"
              (=> minor-gcs (+ num)) " GCs (major/minor)"))
         (subnum (lambda (match name)
                   (or (and-let* ((val (irregex-match-substring match name)))
                         (string->number val))
                       0))))
    (lambda (line)
      (let ((tokens (map string-trim-both (string-split line ",")))
            (result (make-bench-result 0 0 0 0 0 0)))
        (for-each
         (lambda (token)
           (cond
            ((irregex-match cpu-time-pattern token)
             => (lambda (match)
                  (bench-result-cpu-time-set! result (subnum match 'cpu-time))))
            ((irregex-match mutations-pattern token)
             => (lambda (match)
                  (bench-result-mutations-set! result (subnum match 'mutations))
                  (bench-result-mutations-tracked-set! result (subnum match 'mutations-tracked))))
            ((irregex-match major-gcs-time-pattern token)
             => (lambda (match)
                  (bench-result-major-gcs-time-set! result (subnum match 'major-gcs-time))))
            ((irregex-match gcs-pattern token)
             => (lambda (match)
                  (bench-result-minor-gcs-set! result (subnum match 'minor-gcs))
                  (bench-result-major-gcs-set! result (subnum match 'major-gcs))))))
         tokens)
        result))))

(define (compile prog)
  (run-shell-command (sprintf "~a ~a ~a" (csc) (csc-options) prog)))

(define (run bin)
  ;; Return a list of bench-result objects
  (let loop ((n (repetitions))
             (results '()))
    (if (zero? n)
        results
        (let-values (((status output _)
                      (run-shell-command
                       (if (equal? (runtime-options) "")
                           (make-pathname "." bin)
                           (sprintf "~a ~a"
                                    (make-pathname "." bin)
                                    (runtime-options))))))
          (loop (- n 1)
                (if (zero? status)
                    (let ((time-line (last (string-split output "\n"))))
                      (cons (parse-time-output (string-chomp time-line))
                             results))
                    (cons (make-failure-bench-result)
                          results)))))))


(define 1st-col-width 3)
(define 2nd-col-width 23)

(define col-padding "  ")

(define cols
  (map (lambda (label)
         (cons label (string-length label)))
       '("BT [1]"
         "CT [2]"
         "MGT[3]"
         "Mut[4]"
         "MT [5]"
         "MGC[6]"
         "mGC[7]")))

(define (display-header)
  (print "Columns legend:

BT [1] => Build time (seconds)
CT [2] => CPU time (seconds)
MGT[3] => Major GCs time (seconds)
Mut[4] => number of mutations
MT [5] => number of tracked mutations
MGC[6] => number of major GCs
mGC[7] => number of minor GCs
")
  (display (string-append
            (make-string 1st-col-width)
            (string-pad-right "Programs" 2nd-col-width)))
  (for-each (lambda (col)
              (display col)
              (display col-padding))
            (map car cols))
  (newline)
  (flush-output))

(define (display-result/prog prog progno)
  (display
   (string-append
    (string-pad-right
     (sprintf "~a" progno) 1st-col-width #\space)
    (string-pad-right prog 2nd-col-width #\.)))
  (flush-output))

(define (average results accessor)
  (let ((len (length results))
        (sum (apply + (map accessor results))))
    (/ sum (exact->inexact len))))

(define (maybe-drop-.0 num)
  (if (and (inexact? num) (integer? num))
      (inexact->exact num)
      num))

(define (make-failure-results)
  (make-list 6 #f))

(define (make-failure-bench-result)
  (apply make-bench-result (make-failure-results)))

(define (failure-bench-result? bench-result)
  (not (bench-result-cpu-time bench-result)))

(define (display-results compile-time results)
  (let ((vals
         (if (failure-bench-result? (car results))
             (cons compile-time (make-failure-results))
             (map maybe-drop-.0
                  (list compile-time
                        (average results bench-result-cpu-time)
                        (average results bench-result-major-gcs-time)
                        (average results bench-result-mutations)
                        (average results bench-result-mutations-tracked)
                        (average results bench-result-major-gcs)
                        (average results bench-result-minor-gcs))))))
    (for-each (lambda (val idx)
                (display (string-pad-right
                          (if val
                              (->string val)
                              "FAIL")
                          (cdr (list-ref cols idx))
                          #\space))
                (display col-padding))
              vals
              (iota (length vals)))
    (newline)
    (flush-output)))

(define (global-counts results)
  (let loop ((results results)
             (compile-time 0)
             (cpu-time 0)
             (major-gcs-time 0)
             (mutations 0)
             (mutations-tracked 0)
             (minor-gcs 0)
             (major-gcs 0)
             (failures 0))
    (if (null? results)
        `((compile-time      . ,compile-time)
          (cpu-time          . ,cpu-time)
          (major-gcs-time    . ,major-gcs-time)
          (mutations         . ,mutations)
          (mutations-tracked . ,mutations-tracked)
          (minor-gcs         . ,minor-gcs)
          (major-gcs         . ,major-gcs)
          (failures          . ,failures))
        (let* ((result (car results))
               (result-objs (cddr result))
               (failure? (failure-bench-result? (car result-objs)))
               (maybe-sum (lambda (accessor)
                            (if failure?
                                0
                                (apply + (map accessor result-objs))))))
          (loop (cdr results)
                (+ compile-time (cadr result))
                (+ cpu-time (maybe-sum bench-result-cpu-time))
                (+ major-gcs-time (maybe-sum bench-result-major-gcs-time))
                (+ mutations (maybe-sum bench-result-mutations))
                (+ mutations-tracked (maybe-sum bench-result-mutations-tracked))
                (+ minor-gcs (maybe-sum bench-result-minor-gcs))
                (+ major-gcs (maybe-sum bench-result-major-gcs))
                (+ failures (if failure? 1 0)))))))

(define (write-log! results)
  ;; `results' is a list of ("prog" <build-time> <bench-result1> ...)
  (with-output-to-file (log-file)
    (lambda ()
      (pp `((log-format-version . 1)
            (repetitions . ,(repetitions))
            (installation-prefix . ,(installation-prefix))
            (csc-options . ,(csc-options))
            (runtime-options . ,(runtime-options))
            (results . ,(map (lambda (result)
                               (let ((prog (car result))
                                     (compile-time (cadr result))
                                     (results (cddr result)))
                                 (append (list prog compile-time)
                                         (map bench-result->alist
                                              results))))
                             results))))))
  (global-counts results))


(define (display-env num-progs)
  (print #<#EOF
Repeating each program #(repetitions) times
Using #(csc) #(csc-options)
#(if (equal? (runtime-options) "") "" (conc "Runtime options: " (runtime-options) "\n"))
Total number of programs to benchmark: #num-progs

The values displayed correspond to the arithmetic mean of
all results (except build time).

EOF
))

(define (prettify-time seconds)
  (define (pretty-time seconds millisecs)
    (cond ((zero? seconds)
           "")
          ((< seconds 60)
           (conc (+ seconds millisecs) "s"))
          ((< seconds 3600)
           (let ((mins (quotient seconds 60)))
             (conc mins "m" (pretty-time (- seconds (* 60 mins))
                                         millisecs))))
          (else
           (let ((hours (quotient seconds 3600)))
             (conc hours "h" (pretty-time (- seconds (* 3600 hours))
                                          millisecs))))))
  (if (zero? seconds)
      "0s"
      (let* ((exact (inexact->exact (truncate seconds)))
             (millisecs (- seconds exact)))
        (if (zero? exact)
            (conc millisecs "s")
            (pretty-time exact millisecs)))))


(define (run-all)
  (let* ((here (current-directory))
         (num-progs (length (programs)))
         (all-results '())
         (add-results!
          (lambda (prog compile-time results)
            (set! all-results
              (cons (cons prog (cons compile-time results))
                    all-results)))))
    (change-directory (programs-dir))
    (display-env num-progs)
    (display-header)
    (let loop ((progs (programs))
               (progno 1))
      (unless (null? progs)
        (let* ((prog (car progs))
               (bin (pathname-strip-extension prog)))
          (display-result/prog bin progno)
          (let-values (((status output compile-time) (compile prog)))
            (let ((results (and (zero? status) (run bin))))
              (cond (results
                     (add-results! bin compile-time results)
                     (display-results compile-time results))
                    (else (fprintf (current-error-port) "FAIL\n"))))))
        (loop (cdr progs) (+ 1 progno))))
    (change-directory here)
    (write-log! all-results)))

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
Usage: #program [ <options> ] [ config file ]

<options> are:
  --programs-dir=<directory>      directory where programs are
  --log-file=<file>               the log filename
  --debug-file=<file>             file to log debug info to
  --repetitions=<number>          number of times to repeat each program
  --csc-options=<csc options>     options to give csc when compiling programs
  --runtime-options=<options>     runtime options
  --programs=<prog1>,<prog2>      a comma-separated list of programs to run
  --skip-programs=<prog1>,<prog2> a comma-separated list of programs to skip

EOF
    port)
    (when exit-code (exit exit-code))))

(let ((args (command-line-arguments)))
  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (let ((args-without-options
         (remove (lambda (arg)
                   (substring=? "--" arg))
                 args)))
    (unless (null? args-without-options)
      ;; load config file
      (load (car args-without-options))))

  ;; Set parameters according to options passed via command line (they
  ;; clobber config file options)
  (programs-dir (or (cmd-line-arg '--programs-dir args) (programs-dir)))
  (log-file (or (cmd-line-arg '--log-file args) (log-file)))
  (debug-file (and-let* ((df (or (cmd-line-arg '--debug-file args) (debug-file))))
                (if (absolute-pathname? df)
                    df
                    (make-pathname (current-directory) df))))
  (csc-options (or (cmd-line-arg '--csc-options args) (csc-options)))
  (runtime-options (or (cmd-line-arg '--runtime-options args) (runtime-options)))
  (repetitions (or (and-let* ((r (cmd-line-arg '--repetitions args)))
                     (string->number r))
                   (repetitions)))

  ;; Don't clobber log files
  (when (file-exists? (log-file))
    (fprintf (current-error-port)
             "'~a' already exists.  Won't clobber it.  Aborting.\n"
             (log-file))
    (exit 1))

  ;; Determine programs to be run
  (programs (cond ((cmd-line-arg '--programs args)
                   => (lambda (progs)
                        (map string->symbol (string-split progs ","))))
                  (else (or (programs) (all-progs)))))

  (skip-programs (append
                  (skip-programs)
                  (cond ((cmd-line-arg '--skip-programs args)
                         => (lambda (progs)
                              (map string->symbol (string-split progs ","))))
                        (else '()))))

  ;; Remove skipped programs
  (programs (if (null? (skip-programs))
                (programs)
                (remove (lambda (prog)
                          (memq prog (skip-programs)))
                        (programs))))

  ;; Set the correct filename
  (programs (map (lambda (prog)
                   (make-pathname #f (->string prog) "scm"))
                 (programs)))

  ;; Mark the current benchmark in the debug file
  (when (debug-file)
    (with-output-to-file (debug-file)
      (lambda ()
        (print "======================== " (time->string (seconds->local-time)))
        (print "csc options: " (csc-options))
        (print "Runtime options: " (runtime-options))
        (newline))
      append:))

  (when (installation-prefix)
    (set-environment-variable! "LD_LIBRARY_PATH" (make-pathname (installation-prefix) "lib")))

  (let ((counts (run-all)))
    (print #<#EOF

Total compile time:             #(prettify-time (alist-ref 'compile-time counts))
Total run time (CPU time):      #(prettify-time (alist-ref 'cpu-time counts))
Total time spent in major GCs:  #(prettify-time (alist-ref 'major-gcs-time counts))
Total mutations:                #(alist-ref 'mutations counts)
Total mutations tracked:        #(alist-ref 'mutations-tracked counts)
Total number of minor GCs:      #(alist-ref 'minor-gcs counts)
Total number of major GCs:      #(alist-ref 'major-gcs counts)
Total number failures:          #(alist-ref 'failures counts)
EOF
)))
