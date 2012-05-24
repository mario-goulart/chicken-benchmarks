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



(define (display-results prog times)
  (display (string-pad-right prog 20 #\_))
  (for-each
   (lambda (time)
     (display (string-pad (->string (or time "FAIL")) results/pad #\_)))
   times)
  (newline))


(define-record log repetitions installation-prefix csc-options results)


(define (read-log log-file)
  (let ((log-data (with-input-from-file log-file read)))
    (make-log (alist-ref 'repetitions log-data)
              (alist-ref 'installation-prefix log-data)
              (alist-ref 'csc-options log-data)
              (alist-ref 'results log-data))))


(define (compare logs)
  (let ((progs (map car (log-results (car logs)))))
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
