(define-syntax repeat
  (syntax-rules ()
    ((_ n forms ...)
     (let loop ((i n))
       (unless (zero? i)
         forms ...
         (loop (- i 1)))))))

(define list-of-strings
  '("foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo" "foo"))


(define big-string
  "ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssfoossssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss")
