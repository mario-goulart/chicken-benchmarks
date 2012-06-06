#! /bin/sh
#|
exec csi -s $0 "$@"
|#

(use posix utils srfi-1)

;;; Configurable parameters
(define repetitions (make-parameter 10))
(define debug? (make-parameter #f))
(define installation-prefix (make-parameter #f)) ;; #f -> use chicken tools from PATH
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
             string<)))


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


(define (usage #!optional exit-code)
  (printf "Usage: ~a [ config file ]\n"
          (pathname-strip-directory (program-name)) )
  (when exit-code (exit exit-code)))

(let ((args (command-line-arguments)))
  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (unless (null? args) ;; load config file
    (load (car args)))

  ;; Determine programs to be run
  (programs (or (programs) (all-progs)))

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
