(use srfi-13)
(include "utils/utils.scm")

(time (repeat 100000 (string-drop (string-drop big-string 100) 20)))
