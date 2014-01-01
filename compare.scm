#! /bin/sh
#|
exec csi -s $0 "$@"
|#

(use posix srfi-13 srfi-1)

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

  (print "Displaying normalized results (larger numbers indicate better results)\n")

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
  (and (##sys#tty-port? (current-output-port))
       (member (get-environment-variable "TERM")
               '("xterm" "xterm-color" "xterm-256color" "rxvt" "kterm"
                 "linux" "screen" "screen-256color" "vt100"))))

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


(define-record log repetitions installation-prefix csc-options results)


(define (read-log log-file)
  (let ((log-data (with-input-from-file log-file read)))
    (make-log (alist-ref 'repetitions log-data)
              (alist-ref 'installation-prefix log-data)
              (alist-ref 'csc-options log-data)
              (alist-ref 'results log-data))))


(define (compare logs)
  (let ((progs (sort (map car (log-results (car logs))) string<)))
    (display-header logs)
    (for-each (lambda (prog)
                (display-results prog
                                 (map (lambda (log)
                                        (let ((results (log-results log)))
                                          (alist-ref prog results equal?)))
                                      logs)))
              progs)))


(define (usage #!optional exit-code)
  (printf "Usage: ~a log-file-1 log-file-2 ..."
          (pathname-strip-directory (program-name)))
  (when exit-code (exit exit-code)))

(let ((args (command-line-arguments)))
  (when (null? args)
    (usage 1))

  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (compare (map read-log args)))
