(use srfi-13)
(include "utils/utils.scm")

(time (repeat 100000 (string-take-right (string-take-right big-string 100) 20)))
