;;   Ray tracer for Chicken Scheme.
;;   Converted from the original in C at
;;   https://wiki.cs.auckland.ac.nz/enggen131/index.php/User:Dols008
;;
;; This program conses a lot, triggering a huge number of
;; garbage collections, thus it serves as a good GC benchmark.

(define-constant Width 300)
(define-constant Height Width)

(define (square x)  (* x x))

(define (add list1 list2)
  (map + list1 list2))

(define (sub list1 list2)
  (map - list1 list2))

(define (scale seq n)
  (map  (cut * <> n)  seq))

(define (mul list1 list2)
  (map * list1 list2))

(define (dot list1 list2)
  (apply + (mul list1 list2)))

(define (squared-length alist)
  (dot alist alist))

(define (normal alist)
  (let ((len (sqrt (squared-length alist))))
    (map  (cut / <> len)  alist)))


(define-record Ray pos dir)

(define-record Light pos color)

(define-record Sphere pos radius color shine reflect)

(define (ray-hit-sphere sphere ray)
  (let* ((diff (sub (Sphere-pos sphere) (Ray-pos ray)))
         (proj (dot diff (Ray-dir ray)))
         (closest (add (Ray-pos ray) (scale (Ray-dir ray) proj)))
         (tangent (sub closest (Sphere-pos sphere)))
         (sq-tangent-length (squared-length tangent))
         (sq-radius (square (Sphere-radius sphere))))
    (if (> sq-tangent-length sq-radius)
      0
      (- proj (sqrt (- sq-radius sq-tangent-length))))))



(define (calc-lighting pos norm ray sphere light)
  (let* ((rel (normal (sub (Light-pos light) pos)))
         (diffuse (max (dot rel norm) 0))
         (diff-col (scale (Light-color light) diffuse))
         (eye (sub (Ray-pos ray) pos))
         (half (normal (add eye rel)))
         (specular (dot half norm))
         (specular (expt (max specular 0) 64))
         (spec-col (scale (Light-color light) specular)))
    (add (mul (Sphere-color sphere) diff-col)
         (scale spec-col (Sphere-shine sphere)))))



(define NUM_SPHERES 7)
(define NUM_LIGHTS  3)
(define spheres (make-vector NUM_SPHERES ))
(define lights (make-vector NUM_LIGHTS ))



(define (build-scene)
  (do ((i 0 (add1 i)))
      ((= i 5))
    (let* ((theta (* 0.4 (- i 2)))
           (pos (list (* 3 (sin theta)) (* -3 (cos theta)) 5.0)))
      (vector-set! spheres i (make-Sphere pos 1 '(0.8 0.1 0.1) 0.2 0))))

  (vector-set! spheres 5 (make-Sphere
    '(-3 1 5) 2 '(1 1 0.99) 0.5 1.0 ))
  (vector-set! spheres 6 (make-Sphere
    '(3 -3 15) 8 '(0.75 0.5 0) 0.5 1.0 ))

  (vector-set! lights 0 (make-Light '(2 2 1) '(1 1 1) ))
  (vector-set! lights 1 (make-Light '(-4 0 5) '(0.1 0.5 0.1)) )
  (vector-set! lights 2 (make-Light  '(4 0 5) '(0.1 0.5 0.1))))



(define MAX_RECURSION_DEPTH  2)


(define (trace ray depth )
  (let ((hit #f)
        (color (list 0 0 0))
        (dist 0))
    (do ((i 0 (add1 i)))
        ((= i NUM_SPHERES))
      (let ((d (ray-hit-sphere (vector-ref spheres i) ray)))
        (when (> d 0)
          (when (or (not hit) (< d dist))
            (set! dist d)
            (set! hit i)))))
    (if hit
      (let* ((pos (add (Ray-pos ray) (scale (Ray-dir ray) dist)))
             (norm (normal (sub pos
                                (Sphere-pos (vector-ref spheres hit))))))
        (do ((i 0 (add1 i)))
            ((= i NUM_LIGHTS))
          (set! color (add color
            (calc-lighting pos norm ray (vector-ref spheres hit)
                                        (vector-ref lights i)))))
        (when (< depth MAX_RECURSION_DEPTH)
          (let ((reflect-ray (make-Ray pos
                  (sub (Ray-dir ray)
                       (scale norm (* 2 (dot (Ray-dir ray) norm)))))))
            (set! color
              (add color
                   (scale (trace reflect-ray (+ 1 depth))
                          (Sphere-reflect (vector-ref spheres hit)))))))
        color)
      (scale (add (Ray-dir ray) '(1 1 1)) 0.125))))


(define r (make-Ray '(0 0 0) '(0 0 0)))
(define color '(0 0 0))
(define image '())

(build-scene)

(define (main)
  (do ((y 0 (add1 y)))
      ((= y Height))
    (begin
      (do ((x 0 (add1 x)))
          ((= x Width))
        (begin
          (Ray-dir-set! r
                        (normal
                         (list (- (/ x Width) 0.5)
                               (- (- (/ y Height) 0.5))
                               0.5)))
          (set! color (trace r 0))
          (set! color
            (map  (lambda (c) (min c 1))  color))
          (set! image (cons color image)))))))

(time (main))
