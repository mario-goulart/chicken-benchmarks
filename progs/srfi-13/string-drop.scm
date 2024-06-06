(cond-expand
 (chicken-4 (use srfi-13))
 ((or chicken-5 chicken-6) (import srfi-13))
 (else (error "Unsupported CHICKEN version.")))
(include "utils/utils.scm")

(time (repeat 100000 (string-drop (string-drop big-string 100) 20)))
