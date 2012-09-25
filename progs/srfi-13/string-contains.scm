(use srfi-13)

(include "utils/utils.scm")

(time (repeat 10000 (string-contains big-string "foo")))
