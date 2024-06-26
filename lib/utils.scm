(import (except scheme log))
(cond-expand
 (chicken-4
  (use data-structures extras srfi-1 srfi-13))
 ((or chicken-5 chicken-6)
  (import (chicken format)
          (chicken string))
  (import srfi-1 srfi-13))
 (else
  (error "Unsupported CHICKEN version.")))

(define (die! fmt . args)
  (apply fprintf (cons (current-error-port)
                       (cons (string-append (string-chomp fmt "\n") "\n")
                             args)))
  (exit 1))

(define (parse-cmd-line cmd-line-args spec)
  ;; spec: list of elements.  The format of elements is:
  ;;  - symbols: options that do not require an argument
  ;;  - lists: options that require an argument.
  ;;  - what doesn't match these patterns is assumed to be a not-named arg
  ;; Return a pair ((<not-named args>) . (<opt> . <val>) ...), where <val> for
  ;; options which don't require an argument is #t.
  ;; Note: options are supposed to start with `--'.  -h and -help are
  ;;       specially handled.
  (let ((nonamed '())
        (parsed-opts '()))
    (let loop ((args cmd-line-args))
      (unless (null? args)
        (let ((arg (car args)))
          (cond ((string-prefix? "--" arg)
                 (if (substring-index "=" arg)
                     (let* ((parts (string-split arg "="))
                            (param (string->symbol (car parts)))
                            (val (string-intersperse (cdr parts) "=")))
                       (if (memq param spec)
                           (die! "~a does not take any argument." param)
                           (if (alist-ref param (filter pair? spec))
                               (set! parsed-opts (cons (cons param val) parsed-opts))
                               (die! "Invalid option: ~a" param))))
                     (let ((param (string->symbol arg)))
                       (if (memq param spec)
                           (set! parsed-opts (cons (cons param #t)  parsed-opts))
                           (if (alist-ref param (filter pair? spec))
                               (die! "~a requires an argument." param)
                               (die! "Invalid option: ~a" param))))))
                ((string=? arg "-h")
                 (set! parsed-opts (cons (cons '-h #t) parsed-opts)))
                ((string=? arg "-help")
                 (set! parsed-opts (cons (cons '-help #t) parsed-opts)))
                (else
                 (set! nonamed (cons arg nonamed)))))
        (loop (cdr args))))
    (cons (reverse nonamed) parsed-opts)))

(define (cmd-line-arg option parsed-opts #!key multiple?)
  ;; Returns the argument associated to the command line option OPTION
  ;; in ARGS or #f if OPTION is not found in ARGS.  ARGS is the cdr of
  ;; the pair returned by parse-cmd-line.  If `multiple?' is #t and the
  ;; option requires a value, a list of values bound to the given option
  ;; is returned.
  (let ((val
         (let loop ((opts parsed-opts))
           (if (null? opts)
               (if multiple?
                   '()
                   #f)
               (let* ((opt (car opts))
                      (param (if (pair? opt)
                                 (car opt)
                                 opt)))
                 (cond ((and (pair? opt) (eq? (car opt) option))
                        (if multiple?
                            (cons (cdr opt) (loop (cdr opts)))
                            (cdr opt)))
                       ((and (symbol? opt) (eq? opt option))
                        #t)
                       (else (loop (cdr opts)))))))))
    (if (list? val)
        (reverse val)
        val)))

(define (help-requested? args)
  (or (cmd-line-arg '-h args)
      (cmd-line-arg '-help args)
      (cmd-line-arg '--help args)))

(define (truncate* n)
  (if (integer? n)
      n
      (/ (truncate (* n 100.0)) 100)))
