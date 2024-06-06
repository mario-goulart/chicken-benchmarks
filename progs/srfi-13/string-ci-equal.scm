(cond-expand
 (chicken-4 (use srfi-13))
 ((or chicken-5 chicken-6) (import srfi-13))
 (else (error "Unsupported CHICKEN version.")))
(include "utils/utils.scm")

(time (repeat 300000 (string-ci= big-string big-string)))
