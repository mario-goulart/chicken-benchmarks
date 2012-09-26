(use srfi-13)
(include "utils/utils.scm")

(time (repeat 300000 (string= big-string big-string)))
