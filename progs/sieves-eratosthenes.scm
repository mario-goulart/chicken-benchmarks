;; From http://www.reddit.com/r/scheme/comments/1u2ac6/scheme_interpreter_speed_comparison/

(define (primes<2n n)
  (erato 2 (* n 2) (list)))

(define (erato n z primes)

  (define (sieve s)
    (if (or (null? s)                     ; prime
            (< n (* (car s) (car s))))    ; prime!
        (erato (+ n 1) z (append primes (list n)))
        (if (zero? (modulo n (car s)))  ; composite
            (erato (+ n 1) z primes)
            (sieve (cdr s)))))

  (if (< n z)
      (sieve primes)
      primes))

(time (primes<2n 40000))
