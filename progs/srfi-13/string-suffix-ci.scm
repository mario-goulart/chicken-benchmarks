(use srfi-13)
(include "utils/utils.scm")

(time (repeat 1000000 (string-suffix-ci? "foo" "askkkkkkkkkkkasas dasdasdasda")))
