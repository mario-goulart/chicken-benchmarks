(use srfi-13)

(include "utils/utils.scm")

(time (repeat 500000 (string-concatenate list-of-strings)))
