(use srfi-13)
(include "utils/utils.scm")

(time (repeat 100000 (string-take (string-take big-string 100) 20)))
