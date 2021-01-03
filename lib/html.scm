(import (chicken format)
        (chicken port))
(import srfi-1 sxml-transforms)

(define chart-width 500)
(define chart-text-space 150)
(define bar-height 19)
(define bar-space (+ 1 bar-height))

(define sxml->html
  (let ((rules `((literal *preorder* . ,(lambda (t b) b))
                 . ,universal-conversion-rules*)))
    (lambda (sxml)
      (with-output-to-string
        (lambda ()
          (SRV:send-reply (pre-post-order* sxml rules)))))))

(define colors
  '(deepskyblue
    red
    gold
    magenta
    green
    lightgray
    turquoise
    cyan
    fuchsia
    beige
    gray))

(define css #<<ENDCSS
body { font-size: 12pt; background-color: white; }
.chart text { font-size: 10pt; }
.best { font-weight: bold; color: green; }
.worst { color: red; }
table.zebra tr.odd { background-color:#FFC; }
table.zebra td { padding: 4px; }
ENDCSS
)

(define (zebra-table header rows)
  `(table (@ (class "zebra"))
          ,(if header
               `(tr ,@(map (lambda (h) `(th ,h)) header))
               '())
          ,(let ((odd-row #f))
             (map (lambda (row)
                    (set! odd-row (not odd-row))
                    `(tr (@ (class ,(if odd-row "odd" "even")))
                         ,@(map (lambda (cell) `(td ,cell)) row)))
                  rows))))

(define (fit-to-chart val max)
  (inexact->exact (round (/ (* 1.0 (- chart-width chart-text-space) val) max))))

(define (%make-bar idx size text)
  `(g (@ (transform ,(sprintf "translate(0, ~a)" (* idx bar-space))))
      (rect (@ (width ,size)
               (height ,bar-height)
               (style ,(sprintf "fill:~a" (list-ref colors idx)))))
      (text (@ (x ,(+ size 5))
               (y "9.5")
               (dy ".35em"))
            ,text)))

(define (make-bar idx val max text)
  (let* ((bar-size (and val
                        (if (or (zero? val) (zero? max))
                            0
                            (fit-to-chart val max)))))
    (%make-bar idx
               (if val bar-size 0)
               text)))

(define (plot-chart data #!key (unit-printer identity))
  (let ((max (apply max (map cadr data))))
    `(svg (@ (class "chart")
             (width ,chart-width)
             (height ,(* bar-space (length data))))
          ,@(map (lambda (item item-idx)
                   (let ((label (car item))
                         (val (cadr item)))
                     (make-bar item-idx val max (unit-printer val))))
                 data
                 (iota (length data))))))

(define (generate-html data)
  (sxml->html
   `((literal "<!doctype html>")
     (html
      (head
       (title "CHICKEN benchmarks")
       (style ,css))
      (body ,data)))))
