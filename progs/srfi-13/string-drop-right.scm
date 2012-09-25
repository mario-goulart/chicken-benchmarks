(use srfi-13)
(include "utils/utils.scm")

(time (repeat 100000 (string-drop-right (string-drop-right big-string 100) 20)))
