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
  (let* ((p (open-input-pipe (string-append command " 2>&1")))
         (output (read-all p)))
    (when (debug?) (print "Running " command))
    (values (arithmetic-shift (close-input-pipe p) -8)
            output)))


(define (parse-time time-output)
  (let* ((time-line (last (string-split time-output "\n")))
         (time-tokens (string-split time-line)))
    (string->number (string-chomp (car time-tokens) "s"))))


(define (compile prog)
  (run-shell-command (sprintf "~a ~a ~a" (csc) (csc-options) prog)))


(define (run bin)
  ;; Return the total time in case of normal execution or #f in case
  ;; of failure
  (let loop ((n (repetitions))
             (total-time 0))
    (if (zero? n)
        total-time
        (let-values (((status output) (run-shell-command (make-pathname "." bin))))
          (and (zero? status)
               (loop (- n 1)
                     (+ total-time (parse-time output))))))))


(define (add-result! prog time)
  (set! *results* (cons (cons prog time)
                        *results*)))


(define (display-result/prog prog progno num-progs)
  (display
   (string-append
    (string-pad-right (sprintf "(~a/~a)" progno num-progs) 8 #\space)
    (string-pad-right prog 40 #\.)))
  (flush-output))


(define (display-result/time time)
  (print (if time
             (conc time "s")
             "FAIL")))


(define (write-log!)
  (with-output-to-file (log-file)
    (lambda ()
      (pp `((repetitions . ,(repetitions))
            (installation-prefix . ,(installation-prefix))
            (csc-options . ,(csc-options))
            (results . ,*results*))))))


(define (display-env)
  (printf "Repeating each program ~a times\n" (repetitions))
  (printf "Using ~a ~a\n" (csc) (csc-options))
  (newline))


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
    (display-env)
    (let loop ((progs (programs))
               (progno 1))
      (unless (null? progs)
        (let* ((prog (car progs))
               (bin (pathname-strip-extension prog)))
          (display-result/prog bin progno num-progs)
          (let-values (((status output) (compile prog)))
            (let ((result (and (zero? status) (run bin))))
              (add-result! bin result)
              (display-result/time result))))
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
  (printf
   (string-append
    "Usage: ~a [ <options> ] [ config file ]\n"
    "\n"
    "<options> are:\n"
    "  --programs-dir=<directory>      directory where programs are\n"
    "  --log-file=<file>               the log filename\n"
    "  --repetitions=<number>          number of times to repeat each program\n"
    "  --csc-options=<csc options>     options to give csc when compiling programs\n"
    "  --programs=<prog1>,<prog2>      a comma-separated list of programs to run\n"
    "  --skip-programs=<prog1>,<prog2> a comma-separated list of programs to skip\n"
    "\t\n")
   (pathname-strip-directory (program-name)) )
  (when exit-code (exit exit-code)))

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
