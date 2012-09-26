(use srfi-13)
(include "utils/utils.scm")

(time (repeat 300000 (string-ci= big-string big-string)))
