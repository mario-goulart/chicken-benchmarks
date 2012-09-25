(use srfi-13)
(include "utils/utils.scm")

(time (repeat 1000000 (string-prefix? "foo" "askkkkkkkkkkkasas dasdasdasda")))
