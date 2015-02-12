#! /bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

(module run-benchmarks ()

(import chicken scheme)

(use data-structures extras files posix utils srfi-1 srfi-13 irregex)
(use (only setup-api program-path))

;;; Configurable parameters
(define repetitions (make-parameter 10))
(define debug? (make-parameter #f))
(define installation-prefix (make-parameter (pathname-directory (program-path))))
(define csc-options (make-parameter ""))
(define log-file (make-parameter "benchmark.log"))
(define programs (make-parameter #f)) ;; list of symbols or #f (all programs)
(define skip-programs (make-parameter '())) ;; list of symbols
(define programs-dir (make-parameter "progs"))

(define *results* '())

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
         (output (read-all p))
         (duration (- (current-milliseconds) start)))
    (when (debug?) (print "Running " command))
    (values (arithmetic-shift (close-input-pipe p) -8)
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
  ;; Return a list of bench-result objects in case of normal execution
  ;; or #f in case of failure
  (let loop ((n (repetitions))
             (results '()))
    (if (zero? n)
        results
        (let-values (((status output _) (run-shell-command (make-pathname "." bin))))
          (let ((time-line (last (string-split output "\n"))))
            (and (zero? status)
                 (loop (- n 1)
                       (cons (parse-time-output (string-chomp time-line))
                             results))))))))


(define (add-results! prog compile-time results)
  (set! *results* (cons (cons prog (cons compile-time results))
                        *results*)))

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

(define (display-results compile-time results)
  (let ((vals
         (map maybe-drop-.0
              (list compile-time
                    (average results bench-result-cpu-time)
                    (average results bench-result-major-gcs-time)
                    (average results bench-result-mutations)
                    (average results bench-result-mutations-tracked)
                    (average results bench-result-major-gcs)
                    (average results bench-result-minor-gcs)))))
    (for-each (lambda (val idx)
                (display (string-pad-right
                          (if results
                              (->string val)
                              "FAIL")
                          (cdr (list-ref cols idx))
                          #\space))
                (display col-padding))
              vals
              (iota (length vals)))
    (newline)
    (flush-output)))

(define (write-log!)
  (with-output-to-file (log-file)
    (lambda ()
      (pp `((log-format-version . 1)
            (repetitions . ,(repetitions))
            (installation-prefix . ,(installation-prefix))
            (csc-options . ,(csc-options))
            (results . ,(map (lambda (result)
                               (let ((prog (car result))
                                     (compile-time (cadr result))
                                     (results (cddr result)))
                                 (append (list prog compile-time)
                                         (map bench-result->alist
                                              results))))
                             *results*)))))))


(define (display-env num-progs)
  (print #<#EOF
Repeating each program #(repetitions) times
Using #(csc) #(csc-options)

Total number of programs to benchmark: #num-progs

The values displayed correspond to the arithmetic mean of
all results (except build time).

EOF
))

(define (prettify-time seconds)
  (define (pretty-time seconds)
    (cond ((zero? seconds)
           "")
          ((< seconds 60)
           (conc seconds "s"))
          ((< seconds 3600)
           (let ((mins (quotient seconds 60)))
             (conc mins "m" (pretty-time (- seconds (* 60 mins))))))
          (else
           (let ((hours (quotient seconds 3600)))
             (conc hours "h" (pretty-time (- seconds (* 3600 hours))))))))
  (if (zero? seconds)
      "0s"
      (pretty-time (inexact->exact seconds))))


(define (run-all)
  (let ((here (current-directory))
        (num-progs (length (programs))))
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
              (add-results! bin compile-time results)
              (display-results compile-time results))))
        (loop (cdr progs) (+ 1 progno))))
    (change-directory here)
    (write-log!)))

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
  --repetitions=<number>          number of times to repeat each program
  --csc-options=<csc options>     options to give csc when compiling programs
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
                   (string-prefix? "--" arg))
                 args)))
    (unless (null? args-without-options)
      ;; load config file
      (load (car args-without-options))))

  ;; Set parameters according to options passed via command line (they
  ;; clobber config file options)
  (programs-dir (or (cmd-line-arg '--programs-dir args) (programs-dir)))
  (skip-programs (or (cmd-line-arg '--skip-programs args) (skip-programs)))
  (log-file (or (cmd-line-arg '--log-file args) (log-file)))
  (csc-options (or (cmd-line-arg '--csc-options args) (csc-options)))
  (repetitions (or (and-let* ((r (cmd-line-arg '--repetitions args)))
                     (string->number r))
                   (repetitions)))

  ;; Determine programs to be run
  (programs (cond ((cmd-line-arg '--programs args)
                   => (lambda (progs)
                        (map string->symbol (string-split progs ","))))
                  (else (or (programs) (all-progs)))))

  ;; Remove skipped programs
  (programs (if (null? skip-programs)
                (programs)
                (remove (lambda (prog)
                          (memq prog (skip-programs)))
                        (programs))))

  ;; Set the correct filename
  (programs (map (lambda (prog)
                   (make-pathname #f (->string prog) "scm"))
                 (programs)))


  (when (installation-prefix)
    (setenv "LD_LIBRARY_PATH" (make-pathname (installation-prefix) "lib")))

  (let ((start (current-seconds)))
    (run-all)
    (print "\nTotal run time: "
           (prettify-time (- (current-seconds) start)))))

) ;; end module
