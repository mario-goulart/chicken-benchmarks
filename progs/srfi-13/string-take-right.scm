(cond-expand
 (chicken-4 (use srfi-13))
 (chicken-5 (import srfi-13))
 (else (error "Unsupported CHICKEN version.")))
(include "utils/utils.scm")

(time (repeat 100000 (string-take-right (string-take-right big-string 100) 20)))
