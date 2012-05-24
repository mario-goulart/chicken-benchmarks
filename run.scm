#! /bin/sh
#|
exec csi -s $0 "$@"
|#

(use posix utils srfi-1)

;;; Configurable parameters
(define repetitions (make-parameter 3))
(define debug? (make-parameter #f))
(define installation-prefix (make-parameter #f)) ;; #f -> use chicken tools from PATH
(define csc-options (make-parameter ""))
(define log-file (make-parameter "benchmark.log"))


(define progs-dir "progs")

(define *results* '())

(define progs
  (map pathname-strip-directory
       (glob (make-pathname progs-dir "*.scm"))))


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


(define (display-result prog time)
  (display (string-pad-right prog 40 #\.))
  (print (or time "FAIL")))


(define (write-results!)
  (with-output-to-file (log-file)
    (lambda ()
      (pp *results*))))


(define (display-env)
  (printf "Repeating each program ~a times\n" (repetitions))
  (printf "Using ~a ~a\n" (csc) (csc-options))
  (newline))


(define (run-all)
  (let ((here (current-directory)))
    (change-directory progs-dir)
    (display-env)
    (for-each
     (lambda (prog)
       (let ((bin (pathname-strip-extension prog)))
         (let-values (((status output) (compile prog)))
           (let ((result (and (zero? status) (run bin))))
             (add-result! bin result)
             (display-result bin result)))))
     progs)
    (change-directory here)
    (write-results!)))


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

  (when (installation-prefix)
    (setenv "LD_LIBRARY_PATH" (make-pathname (installation-prefix) "lib")))

  (run-all))
