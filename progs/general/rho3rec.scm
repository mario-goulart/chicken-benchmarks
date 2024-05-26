;; By T.D. Telford (only the max number of iterations reduced), see
;; https://lists.nongnu.org/archive/html/chicken-users/2024-05/msg00000.html
;; for more context.
;;
;; This benchmark program stresses division operations and triggered
;; 69cf7a985c and cfd75ecd9a in chicken-core.

(define (rho n u v c iter prod)
  (let* ( [u1 (modulo (+ (* u u) c) n)]
          [v1 (modulo (+ (* v v) c) n)]
          [v2 (modulo (+ (* v1 v1) c) n)]
          [done #f]
          [max_iter 2000000]
          [iter2 (+ iter 1)]
          [prod2 (modulo (* prod (- u1 v2)) n)] )

    (if (= (modulo iter2 150) 0)
      (begin    ; modulo true
        (let ( [g (gcd prod2 n) ] )
          (if (and (> g 1) (< g n))
            (begin ; factor
              (display "factor = ") (display g) (newline)
              (display "iterations = ") (display iter2) (newline)
              (set! done #t)
            )
            (set! prod2 1) ; no factor
          ) ; end if factor
        ) ; end let
      ) ; end begin for modulo true
      #f ;action for modulo false
    ) ; end major if

    (if (and (< iter2 max_iter) (not done))
      (rho n u1 v2 c iter2 prod2)
      (if done ; either found factor or max iterations
        (display "normal termination \n")
        #f
      ) ; if done
    ) ; if and
  ) ; end let*
)

(let ( [n (- (expt 2 257) 1)] [u 2] [v 11] [c 7] [iter 1] [prod 1] )
    (display "factor n = ") (display n) (newline)
    (time (rho n u v c iter prod))
)
