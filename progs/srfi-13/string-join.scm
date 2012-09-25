(use srfi-13)
(include "utils/utils.scm")

(time (repeat 10000 (string-join list-of-strings ":")))
