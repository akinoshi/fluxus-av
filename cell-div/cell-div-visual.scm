; File: cell-div-visual.scm
; -------------------------
; A script for audio visual performace called 
; Cell Division. This performance is about
; transition from DNA double helix to cell
; division and x chromosome (male/female).
;
; SuperCollider: Control Window (Audio)
; Fluxus: Main Window (Visual)
;
; Author: Akinori Kinoshita
; E-mail: art.akinoshi -at- gmail.com
; Date: Tue Nov 15 12:51:07 CST 2011

;(require fluxus-017/drflux)

(start-audio "SuperCollider:out_1" 256 44100)

(require (lib "csv.ss"))
(require scheme/math)
(require scheme/class)

(clear)

(collisions 1)
(gravity (vector 0 0 0))

; osc setting
(osc-source "12000")
(osc-destination "osc.udp://localhost:57120")

; camera setting
(define M 0.2)
(define K 0.2) ; 2
(define D 0.2) ; 61
(define R 0)

(define ps 0.0)
(define vs 0.0)
(define as 0)
(define f 0)

(define theta 0)

(define rotate? #f)

(define center (build-cube))
(with-primitive center
  (hide 1))
(lock-camera center)

; part-1 (snake) setting ---------------------------------------------------
(define pi 3.141592653589793238462643)
(define 2pi (* 2 pi))
(define n 554)
(define snake-fade 0)
(define speed 1)
(define max-cnt 100)
(define cnt 0)
(define snake 0)
(define snake-flag #t)

(define knot-1 (build-list n
  (lambda (i)
    (let* ((mu (/ (* i 2pi) n))
           (x (+ (* 10 (+ (cos mu) (cos (* 3 mu))))
                 (+ (cos (* 2 mu)) (cos (* 4 mu)))))
           (y (+ (* 6 (sin mu)) (* 10 (sin (* 3 mu)))))
           (z (+ (* 4 (sin (* 3 mu)) (sin (/ (* 5 mu) 2)))
                 (* 4 (sin (* 4 mu)))
                 (- (* 2 (sin (* 6 mu)))))))
      (vmul (vector x y z) 0.6)))))

(define knot-2 (build-list n
  (lambda (i)
    (let* ((mu (/ (* i 2pi) n))
           (x (+ (/ (* 4 (cos (+ mu pi))) 3)
                 (* 2 (cos (* 3 mu)))))
           (y (+ (/ (* 4 (sin mu)) 3)
                 (* 2 (sin (* 3 mu)))))
           (z (+ (sin (* 4 mu))
                 (/ (sin (* 2 mu)) 4))))
      (vmul (vector x y z) 3)))))

(define knot-3 (build-list n
  (lambda (i)
    (let* ((meridian 3)
           (longitude 4)
           (mu (/ (* i 2pi meridian) n))
           (x (* (cos mu)
                 (+ 1 (/ (cos (/ (* longitude mu) meridian)) 2.0))))
           (y (* (sin mu)
                 (+ 1 (/ (cos (/ (* longitude mu) meridian)) 2.0))))
           (z (/ (sin (/ (* longitude mu) meridian)) 2.0)))
      (vmul (vector x y z) 5)))))

(define knot-3-2 (build-list n
  (lambda (i)
    (let* ((meridian 4)
           (longitude 7)
           (mu (/ (* i 2pi meridian) n))
           (x (* (cos mu)
                 (+ 1 (/ (cos (/ (* longitude mu) meridian)) 2.0))))
           (y (* (sin mu)
                 (+ 1 (/ (cos (/ (* longitude mu) meridian)) 2.0))))
           (z (/ (sin (/ (* longitude mu) meridian)) 2.0)))
      (vmul (vector x y z) 5)))))

(define knot-3-3 (build-list n
  (lambda (i)
    (let* ((meridian 6)
           (longitude 11)
           (mu (/ (* i 2pi meridian) n))
           (x (* (cos mu)
                 (+ 1 (/ (cos (/ (* longitude mu) meridian)) 2.0))))
           (y (* (sin mu)
                 (+ 1 (/ (cos (/ (* longitude mu) meridian)) 2.0))))
           (z (/ (sin (/ (* longitude mu) meridian)) 2.0)))
      (vmul (vector x y z) 5)))))

(define granny-knot (build-list n
  (lambda (i)
    (let* ((mu (/ (* i 2pi) n))
           (x (- (* 22 (cos mu))
                 (* 128 (sin mu))
                 (* 44 (cos (* 3 mu)))
                 (* 78 (sin (* 3 mu)))))
           (y (+ (- (* 10 (cos (* 2 mu))))
                 (- (* 27 (sin (* 2 mu))))
                 (* 38 (cos (* 4 mu)))
                 (* 46 (sin (* 4 mu)))))
           (z (+ (* 70 (cos (* 3 mu)))
                 (- (* 40 (sin (* 3 mu)))))))
      (vmul (vector x y z) 0.06)))))

(define cinquefoil-knot (build-list n
  (lambda (i)
    (let* ((mu (/ (* i (* 10 pi)) n))
           (x (* (cos mu) (- 2 (cos (/ (* 2 mu) 5)))))
           (y (* (sin mu) (- 2 (cos (/ (* 2 mu) 5)))))
           (z (- (sin (/ (* 2 mu) 5)))))
      (vmul (vector x y z) 3)))))

(define dummy-sphere (build-sphere 24 24))

(with-primitive dummy-sphere
  (poly-convert-to-indexed)
  (hide 1))

(define sphere (build-list n
  (lambda (i)
    (with-primitive dummy-sphere
      (vmul (pdata-ref "p" i) 10)))))

; prepare snake to move around
;(define snake (build-ribbon (/ n 4)))
(set! snake (build-ribbon (/ n 4)))

(with-primitive snake
  (hint-unlit)
  (hint-vertcols)
  ;(hint-origin)
  (pdata-map! (lambda (w) 0.2) "w"))

(define path knot-1)

(define range (vector 24 24 24))

(define (set-colour)
  (with-primitive snake
    (pdata-index-map!
      (lambda (i c p)
        (hsv->rgb (vector (/ (+ (vx p) (/ (vx range) 2)) (vx range))
                          (/ i (pdata-size))
                          snake-fade
                          (/ i (pdata-size)))))
      "c" "p")))

; '(a b c) -> '(b c a) -> '(c a b)
(define (rotate-list lst)
  (reverse (cons (car lst) (reverse (cdr lst)))))

(define (rotate-snake num)
  (when (> num 0)
    (set! path (rotate-list path))
    (rotate-snake (- num 1))))

(define (rotate-path num)
  (when (> num 0)
    (set! dst-path (rotate-list dst-path))
    (rotate-path (- num 1))))

(define (update-snake num)
  (send-freq)
  (set-colour)
  (rotate-snake (if (equal? face-flag #t) (+ num 1) num))
  (rotate-path (if (equal? face-flag #t) (+ num 1) num))
  (set! cnt (+ cnt 1))
  (when (zero? (modulo cnt 2))
    (set! path (get-new-path path dst-path)))
  (with-primitive snake
    (pdata-index-map!
      (lambda (i p)
        (list-ref path i))
      "p")))

(define dst-path knot-1)

(define (get-new-path src-list dst-list)
  (map (lambda (x y) (vadd (vdiv (vsub y x) max-cnt) x)) src-list dst-list))

; calculate range for the path
(define (set-range lst)
  (vector (- (car (reverse (sort (map (lambda (x) (vx x)) lst) <)))
             (car (sort (map (lambda (x) (vx x)) lst) <)))
          (- (car (reverse (sort (map (lambda (x) (vy x)) lst) <)))
             (car (sort (map (lambda (x) (vy x)) path) <)))
          (- (car (reverse (sort (map (lambda (x) (vz x)) lst) <)))
             (car (sort (map (lambda (x) (vz x)) lst) <)))))

(define range (vector 24 24 24))

(define (send-freq)
  (with-primitive snake
    (osc-send "/freq" "ff" (list (/ (+ (vx (pdata-ref "p" 0)) (/ (vx range) 2)) (vx range))
                                 (/ (+ (vy (pdata-ref "p" 0)) (/ (vy range) 2)) (vy range))))))

; part-2 (cell) setting ----------------------------------------------------
(define div-flag #f)
(define cell-flag #f)
(define blob 0)
(define s 0)
(define ss 0)
(define max-flag #f)
(define str-1 0)
(define str-2 0)

(define dirlight (vtransform (vector 0 1 0) (mrotate (vector 45 45 0))))

(define (toon-light n)
  (let ((lighting (vdot (pdata-get "n" n) dirlight)))
    (when (< lighting 0) (set! lighting 0.1))
    (pdata-set "t" n (vector lighting 0 0)))
  (if (< n 1)
      0
      (toon-light (- n 1))))

(define (deform n)
  (pdata-set "p" n (vadd (pdata-get "pref" n)
    (if #t
      (vadd
        (vmul (pdata-get "nref" n) 
              (* (sin (* (+ (vector-ref (pdata-get "p" n) 1) (* (time) 0.3)) str-1)) ; 9
                 (if (> (gh 0) 1) 1 (gh 0))))
        (vmul (pdata-get "nref" n) 
              (* (sin (* (+ (vector-ref (pdata-get "p" n) 2) (* (time) 0.3)) str-2)) ; 9
                 (if (> (gh 1) 1) 1 (gh 1)))))
      (vector 0 0 0))))
    (if (< n 1)
        0
        (deform (- n 1))))

(define (deformR n)
  (pdata-set "p" n (vadd (pdata-get "prefR" n)
    (if #t
      (vadd
        (vmul (pdata-get "nrefR" n) 
              (* (sin (* (+ (vector-ref (pdata-get "p" n) 1) (* (time) 0.3)) str-2)) ; 3
                 (if (> (gh 3) 1) 1 (gh 3))))
        (vmul (pdata-get "nrefR" n) 
              (* (sin (* (+ (vector-ref (pdata-get "p" n) 2) (* (time) 0.3)) str-1)) ; 3
                 (if (> (gh 4) 1) 1 (gh 4)))))
      (vector 0 0 0))))
    (if (< n 1)
        0
        (deformR (- n 1))))
        
; create nucleus
#;(set! blob (with-state
  (hint-vertcols)
  (shinyness 50)
  (specular (vector 0.5 0.5 0.5))
  (scale 2.6)
  (translate (vector -2 -1 -1))
  (build-blobby 2 (vector 30 30 30) (vector 4 2 2))))

#;(with-primitive blob
;  (pdata-map! (lambda (c) (vector 1 0 0.4)) "c")
  (pdata-index-map! (lambda (i c)
    (cond ((= i 0) (vmul (vector (/ 71 255) (/ 201 255) (/ 191 255)) 0.4))
          ((= i 1) (vmul (vector (/ 255 255) (/ 87 255) (/ 60 255)) 0.4))))
    "c")
  (pdata-map! (lambda (s) 0.25) "s"))

(define (set-blob)
  ; create nucleus
  (set! blob (with-state
    (hint-vertcols)
    (shinyness 50)
    (specular (vector 0.5 0.5 0.5))
    (scale 2.6)
    (translate (vector -2 -1 -1))
    (build-blobby 2 (vector 30 30 30) (vector 4 2 2))))
  
  (with-primitive blob
    (pdata-index-map! (lambda (i c)
      (cond ((= i 0) (vmul (vector (/ 71 255) (/ 201 255) (/ 191 255)) 0.4))
            ((= i 1) (vmul (vector (/ 255 255) (/ 87 255) (/ 60 255)) 0.4))))
      "c")
    (pdata-map! (lambda (s) 0.25) "s")))

; setup left surface
(define (set-surface)  
  (set! s (build-sphere 24 24))
  (with-primitive s
   (scale 3)
   (hint-unlit)
   (poly-convert-to-indexed)
   (texture (load-texture "gradient.png"))
   ;(colour (vector 0.9 0.5 1))
   (colour (vector 0 0 0))
   (pdata-copy "p" "pref")
   (pdata-copy "n" "nref")))

; setup right surface
(define (set-right-surface)
  (set! ss (build-sphere 24 24))
  (with-primitive ss
   (scale 3)
   (hint-unlit)
   (poly-convert-to-indexed)
   (texture (load-texture "gradient.png"))
   (colour (vector 0.9 0.5 1))
   ;(colour (vector 0 0 0))
   (pdata-copy "p" "prefR")
   (pdata-copy "n" "nrefR")))

(define p (build-particles 50))

(with-primitive p
  (pdata-map! (lambda (p) (vmul (grndvec) 3)) "p")
  (pdata-map! (lambda (c) (vector 1 1 1 0)) "c"))

(define (update-cell)
  ; update cell core
  (with-primitive blob
    (pdata-index-map! (lambda (i p)
      (if (equal? div-flag #f)
        (vector 2 1 1)
        (if (equal? max-flag #t)
            (cond ((= i 0) (vector (- 2 1.25) 1 1))
                  ((= i 1) (vector (+ 2 1.25) 1 1)))
            (cond ((= i 0) (vector (- 2 (* 1.25 (if (> (gh 2) 1) 1 (gh 2)))) 1 1))
                  ((= i 1) (vector (+ 2 (* 1.25 (if (> (gh 2) 1) 1 (gh 2)))) 1 1))))))
      "p"))
  ; update cell surface
  (with-primitive s
    (opacity 0.5)    
    (pdata-copy "pref" "p")
    (deform (pdata-size))
    (recalc-normals 0)
    (toon-light (pdata-size)))
  ; translate cell surface if div-flag is on
  (when (equal? div-flag #t)
    (with-primitive s
      (pdata-index-map!
        (lambda (i p) (vadd p
          (vector (* -1 (if (equal? max-flag #t) 1 (if (> (gh 2) 1) 1 (gh 2)))) 0 0)))
        "p"))
    (with-primitive ss
      (opacity 0.5)    
      (pdata-copy "prefR" "p")
      (deformR (pdata-size))
      (recalc-normals 0)
      (toon-light (pdata-size)))
    (with-primitive ss
      (pdata-index-map!
        (lambda (i p) (vadd p
          (vector (* 1 (if (equal? max-flag #t) 1 (if (> (gh 2) 1) 1 (gh 2)))) 0 0)))
        "p")))
  )

; part-3 (face to face) -------------------------------------------------------------
(define size 10000)
(define face-flag #f)
(define box-flag #f)

(define make-hoc-csv-reader
    (make-csv-reader-maker
        '((separator-chars #\,)
         (strip-leading-whitespace? . #t)
         (strip-trailing-whitespace? . #t))))

(define next-row
    (make-hoc-csv-reader
        (open-input-file "HoC_AnimationData1_v1.0/1000.csv")))

(define next-colour-row
    (make-hoc-csv-reader
        (open-input-file "HoC_AnimationData1_v1.0/1000.csv")))

(define (get-point)
    (list->vector (reverse
        (list-tail (reverse (map string->number (next-row))) 1))))

(define (get-intensity)
    (string->number (car (list-tail (next-colour-row) 3))))

(define next-row-2
    (make-hoc-csv-reader
        (open-input-file "HoC_AnimationData1_v1.0/500.csv")))

(define next-colour-row-2
    (make-hoc-csv-reader
        (open-input-file "HoC_AnimationData1_v1.0/500.csv")))

(define (get-point-2)
    (list->vector (reverse
        (list-tail (reverse (map string->number (next-row-2))) 1))))

(define (get-intensity-2)
    (string->number (car (list-tail (next-colour-row-2) 3))))

(define female (build-particles size))
  
(with-primitive female
    (pdata-map! (lambda (p) (vmul (get-point) 0.2)) "p")
    (pdata-map! (lambda (c) (vector (/ 255 255) (/ 87 255) (/ 60 255) (exact->inexact (/ (get-intensity) 100)))) "c")
    (rotate (vector 0 85 180))
    (translate (vector -18 -30 25)))

(with-primitive female
  (pdata-copy "p" "pref-f"))

; prepare for box positions
(with-primitive female
  (pdata-map! (lambda (p) (vadd (vmul (crndvec) 2.45) (vector 18 30 -25))) "p")
  (pdata-copy "p" "pref-box-f"))

(with-primitive female
  (pdata-map! (lambda (p) (vector 18 30 -25)) "p"))

(define male (build-particles size))

(with-primitive male
    (pdata-map! (lambda (p) (vmul (get-point-2) 0.2)) "p")
    (pdata-map! (lambda (c) (vector (/ 71 255) (/ 201 255) (/ 191 255) (exact->inexact (/ (get-intensity-2) 100)))) "c")
    (rotate (vector 0 -85 180))
    (translate (vector -18 -30 25)))

(with-primitive male
  (pdata-copy "p" "pref-m"))

; prepare for box positions
(with-primitive male
  (pdata-map! (lambda (p) (vadd (vmul (crndvec) 2.45) (vector 18 30 -25))) "p")
  (pdata-copy "p" "pref-box-m"))

(with-primitive male
  (pdata-map! (lambda (p) (vector 18 30 -25)) "p"))

(define (update-face)
  (with-primitive female
    (pdata-index-map! (lambda (i p)
      (if (> (flxrnd) 0.85) 
      (vadd p (vdiv (vsub (pdata-get 
        (if (equal? box-flag #t)
               "pref-box-f" 
              "pref-f") i) p) 25)) p))
      "p")
    (if (equal? box-flag #t)
      (pdata-map! (lambda (c)
        (vadd c (vdiv (vsub three c) 10))) ; 25 is too fast
        "c")
      0))
  (with-primitive male
    (pdata-index-map! (lambda (i p)
      (if (> (flxrnd) 0.85) 
      (vadd p (vdiv (vsub (pdata-get
        (if (equal? box-flag #t)    
         "pref-box-m"
         "pref-m") i) p) 50)) p))
      "p")
    (if (equal? box-flag #t)
      (pdata-map! (lambda (c)
        (vadd c (vdiv (vsub three c) 25)))
        "c")
      0)))

; part-4 (core) -------------------------------------------------------------
(define key-debounce #f)
(define ribbon? #f)
(define ribbons 0)
(define destroy-list '())
(define active? #f)
(define one #(0.91 0.51 0.07))
(define two #(0.34 0.79 0.84))
(define three #(0.43 0.49 0.5))
(define core-flag #f)
(define particles 0)
(define l 0)

(define pos '(
; 1st order (1)
#(0 0 0)
; 2nd order (19)
#(-1 0 -1) #(-1 -1 0) #(-1 -1 1) #(0 -1 1) #(1 0 1)
#(0 1 -1) #(1 1 0) #(-1 1 -1) #(1 1 1) #(-1 -1 -1)
#(1 -1 1) #(1 1 -1) #(1 0 0) #(0 -1 0) #(0 0 -1)
#(1 -1 0) #(0 -1 -1) #(1 0 -1) #(1 -1 -1)
; 3rd order (61)
#(-2 -2 -2) #(-2 -1 -2) #(-2 0 -2) #(-2 1 -2) #(-2 2 -2)
#(-1 2 -2) #(0 2 -2) #(1 2 -2) #(2 2 -2) #(2 2 -1)
#(2 2 0) #(2 2 1) #(2 2 2) #(2 1 2) #(2 0 2)
#(2 -1 2) #(2 -2 2) #(1 -2 2) #(0 -2 2) #(-1 -2 2)
#(-2 -2 2) #(-2 -2 1) #(-2 -2 0) #(-2 -2 -1) #(2 0 0)
#(2 -1 0) #(2 1 0) #(2 1 -1) #(2 1 -2) #(2 1 1)
#(2 0 -1) #(2 0 -2) #(2 0 1) #(2 -1 1) #(2 -1 -1)
#(2 -1 -2) #(2 -2 0) #(2 -2 -1) #(2 -2 -2) #(2 -2 1)
#(1 -2 0) #(1 -2 -1) #(1 -2 -2) #(1 -2 1) #(0 -2 0)
#(0 -2 1) #(0 -2 -1) #(0 -2 -2) #(0 -1 -2) #(0 0 -2)
#(0 1 -2) #(-1 -2 -1) #(-1 -2 0) #(-1 -2 1) #(-1 -2 -2)
#(-1 -1 -2) #(-1 0 -2) #(-1 1 -2) #(1 0 -2) #(1 -1 -2)
#(1 1 -2)
))

(define active-pos '(
; 2nd order (7)
#(0 1 0) #(-1 0 0) #(0 0 1) #(-1 1 0) #(-1 1 1)
#(0 1 1) #(-1 0 1)
; 3rd order
#(-2 0 0) #(-2 0 -1) #(-2 0 1) #(-2 0 2) #(-2 -1 -1)
#(-2 -1 0) #(-2 -1 1) #(-2 -1 2) #(-2 1 -1) #(-2 1 0)
#(-2 1 1) #(-2 1 2) #(-2 2 -1) #(-2 2 0) #(-2 2 1)
#(-2 2 2) #(-1 2 -1) #(-1 2 0) #(-1 2 1) #(-1 2 2)
#(0 2 -1) #(0 2 0) #(0 2 1) #(0 2 2) #(1 2 -1)
#(1 2 0) #(1 2 1) #(1 2 2) #(-1 1 2) #(-1 0 2)
#(-1 -1 2) #(0 1 2) #(0 0 2) #(0 -1 2) #(1 1 2)
#(1 0 2) #(1 -1 2)
))

(define shrink-list (build-list (length active-pos) values))

(define (setup list i)
    (when (not (null? list))
        (begin
            (with-state
                (cond
                    ((= i 0) (colour one))
                    ((and (> i 0) (< i 20)) (colour two))
                    (else (colour three)))
                (translate (car list))
                (let ((obj (build-cube)))
                (set! destroy-list (cons obj destroy-list))
                (if active?
                    (active-box obj)
                    (passive-box obj)))
                ))
            (setup (cdr list) (+ i 1))))

; setup passive-box
;(setup pos 0)

(define (get-pos)
    (let ([t (get-global-transform)])
        (vector (vector-ref t 12)
                (vector-ref t 13)
                (vector-ref t 14))))

(define ribbon%
    (class object%
        (init id_)
        
        (field [id id_])

	(define kill? #f)
 
        (define c (with-state
            (if (< id_ 7)
                (colour two)
                (colour three))
            (translate (list-ref active-pos id_))
            (scale 0.25)
            (build-cube)))

        ;(define c (build-cube))
        (active-box c)
        
        (define r (build-ribbon 64))
        
        (with-primitive r
            (hint-unlit)
            ;(colour #(1 0.25))
            (pdata-index-map!
                (lambda (i w)
                    ;(* .1 (cos (* .5 pi (/ i (pdata-size)))))
                    0.015
                    )
                "w"))

(with-primitive r
    (pdata-map! (lambda (p) (list-ref active-pos id)) "p"))
        
        (define (add-p np)
            (with-primitive r
                (pdata-index-map!
                    (lambda (i p)
                        (if (< i (- (pdata-size) 2))
                            (pdata-ref "p" (add1 i))
                            np))
                    "p")))
	(define/public (kill)
	    (destroy c)
	    (destroy r)
	    (set! kill? #t))
            
        (define/public (update)
	  (when (not kill?)
            (with-primitive c
                (opacity 1))
            (add-p (with-primitive c
                        (get-pos)))
            (when (> (gh 0) .5) ; (> 1 .2)
                0
                (kick c (vmul (crndvec) (flxrnd)))
                ;(twist c (vmul (crndvec) (flxrnd)))
                )
            (let ([p (with-primitive c
                        (get-pos))])
                (kick c (vmul p -.05)))))
        
        (super-new)))

#;(define ribbons (build-list (length active-pos)
                    (lambda (x) (make-object ribbon% x))))

; tweener equation
(define (easeInExpo t b c d)
    (if (zero? t)
        b
        (+ (* c (expt 2 (* 10 (- (/ t d) 1)))) b)))

(define (easeOutExpo t b c d)
    (if (= t d)
        (+ b c)
        (+ (* c (+ (* -1 (expt 2 (* -10 (/ t d)))) 1)) b)))

; get size using tweener
(define (get-size)
    (let ((s (* (list-ref size cube-id)
                (easeOutExpo (- (* 1000 (time)) last-time) 0 1 1000))))
         (if (> s (* (list-ref size cube-id) 0.99))
            (list-ref size cube-id)
            s)))

(define particle%
    (class object%
        (init id_)

        (field [id id_])
        
        (define shrink? #f)
        (define last-time (* 1000 (time)))

        (define (get-scale)
            (let ((s (* 0.75 (easeInExpo (- (* 1000 (time)) last-time) 0 1 1000))))
            (if (> s 0.75)
                0.75
                s)))

        (define/public (setup)
            (set! shrink? #t)
            (set! last-time (* 1000 (time))))

        (define/public (update)
            (with-state
                (if (< id 7)
                    (colour two)
                    (colour three))
                (translate (list-ref active-pos id))
                (when shrink? (scale (- 1 (get-scale))))
                (draw-cube)))

        (super-new)))

#;(define particles (build-list (length active-pos)
                    (lambda (x) (make-object particle% x))))
                
(define (render)
    (for-each
        (lambda (r)
            (send r update))
        ribbons))

(define (render-particles)
    (for-each
        (lambda (p)
            (send p update))
        particles))

(define (check-key) (cond
    ((key-pressed "1")
        (when (not key-debounce)
            (set! key-debounce #t)
            (set! shrink-list '())
            (for-each (lambda (x) (send x setup)) particles)))
    ((key-pressed " ")
        (when (and (not key-debounce) (not (null? shrink-list)))
            (set! key-debounce #t)
            (let* ((i (random (length shrink-list)))
                   (j (list-ref shrink-list i)))
                 (send (list-ref particles j) setup)
                 (set! shrink-list (remove j shrink-list)))))
    ((key-pressed "f")
        (when (and (not key-debounce) (not ribbon?) (null? shrink-list))
            (set! key-debounce #t)
            (set! ribbons (build-list (length active-pos)
                (lambda (x) (make-object ribbon% x))))
            (set! ribbon? #t)
            (osc-send "/free" "f" (list 1))))
    ((key-pressed "a")
        (when (not key-debounce)
            (set! key-debounce #t)
            (destroy-all destroy-list)
            (set! active? #t)
            (setup pos 0)
            (osc-send "/activate" "f" (list 1))))
    (else (set! key-debounce #f))))

(define (destroy-all list)
    (when (not (null? list))
        (destroy (car list))
        (destroy-all (cdr list))))

(define (swing)
    (light-position l (vmul (vector (sin (time)) 0.2
                                    (cos (time))) 50)))

(define (update-core)
  (check-key)
  (swing)
  (if ribbon?
    (render)
    (render-particles)))

; part-5 (polygon) -------------------------------------------------------------
(define polygon-flag #f)
(define polygon-list '())

(define (build-polygon)
  (with-primitive p
    (for ([i (in-range 50)])
      (let ([n (build-polygons 3 'triangle-list)]
            [p0 (pdata-ref "p" (* i 3))]
            [p1 (pdata-ref "p" (+ (* i 3) 1))]
            [p2 (pdata-ref "p" (+ (* i 3) 2))])
           (set! polygon-list (cons n polygon-list))
           (with-primitive n
             (hint-unlit)
             (backfacecull 0)
             (colour (vector (flxrnd) (flxrnd) (flxrnd) 0.3))
             (pdata-set "p" 0 p0)
             (pdata-set "p" 1 p1)
             (pdata-set "p" 2 p2)
             (recalc-normals 0))))))

(define (set-polygon n lst)
  (when (> n 0)
    (with-primitive p
      (let ([p0 (pdata-ref "p" (* n 3))]
            [p1 (pdata-ref "p" (+ (* n 3) 1))]
            [p2 (pdata-ref "p" (+ (* n 3) 2))])
      (with-primitive (car lst)
        (pdata-set "p" 0 p0)
        (pdata-set "p" 1 p1)
        (pdata-set "p" 2 p2)
        (recalc-normals 0))))
      (set-polygon (- n 1) (cdr lst))))

(define (break-polygon)
  (destroy p)
  (for-each (lambda (n)
    (active-box n)
    ;(twist n (vmul (grndvec) 0.05))
    (kick n (vmul (crndvec) (flxrnd))))
    polygon-list)
  (spawn-timed-task
    (+ (time-now) 3)
      (lambda ()
      (for-each (lambda (n) (destroy n)) polygon-list))))

(define (update-polygon)
  #;(with-primitive p
    (pdata-set "p" (round (* size (flxrnd))) (vmul (grndvec) 3)))
  (set-polygon 50 polygon-list))

; control setting -----------------------------------------------------------
(define (check-osc)
  (when (osc-msg "/snake")
    (cond ((= (osc 0) 0) (set! snake-flag #f)
                         (destroy snake))
          ((= (osc 0) 1) (set! snake-flag #t)
                         (set! snake (build-ribbon (/ n 4)))
                         (with-primitive snake
                           (hint-unlit)
                           (hint-vertcols)
                           (pdata-map! (lambda (w) 0.2) "w")))))
  (when (osc-msg "/snake_fade")
    ;(display (osc 0))
    ;(newline)
    (set! snake-fade (osc 0)))
  (when (osc-msg "/path")
    (cond ((= (osc 0) 1) (set! dst-path knot-1))
          ((= (osc 0) 2) (set! dst-path knot-2))
          ((= (osc 0) 3) (set! dst-path knot-3))
          ((= (osc 0) 4) (set! dst-path knot-3-2))
          ((= (osc 0) 5) (set! dst-path knot-3-3))
          ((= (osc 0) 6) (set! dst-path granny-knot))
          ((= (osc 0) 7) (set! dst-path cinquefoil-knot))
          ((= (osc 0) 8) (set! dst-path sphere))))
  (when (osc-msg "/camera")
    (cond ((string=? (osc 0) "h") (set! R (- R 90)))
          ((string=? (osc 0) "l") (set! R (+ R 90)))
          ((string=? (osc 0) "j") (set! R (+ R -90)))
          ((string=? (osc 0) "k") (set! R (+ R -90)))
          ((string=? (osc 0) "a") (set! rotate? (if (equal? rotate? #t) #f #t)))))
  (when (osc-msg "/gain") (gain (osc 0)))
  (when (osc-msg "/cell")
    (cond ((= (osc 0) 0) (set! cell-flag #f)
                         (destroy blob)
                         (destroy s)
                         (destroy ss))
          ((= (osc 0) 1) (set! cell-flag #t)
                         (set-blob)
                         (set-surface))))
  (when (osc-msg "/cell_fade")
    (when (equal? cell-flag #t)
      (with-primitive s
        ;(clear-texture-cache)
        (colour (vector (* 0.9 (osc 0)) (* 0.5 (osc 0)) (osc 0))))
      (when (equal? div-flag #t)
        (with-primitive ss
          (colour (vector (* 0.9 (osc 0)) (* 0.5 (osc 0)) (osc 0)))))))
  (when (osc-msg "/div")
    (cond ((= (osc 0) 0) (set! div-flag #f)
                         (destroy ss))
          ((= (osc 0) 1) (set! div-flag #t)
                         (set-right-surface))))
  (when (osc-msg "/blend")
    (cond ((= (osc 0) 1)
              (when (equal? cell-flag #t)
                    (with-primitive s (blend-mode 'one 'src-color)))
              (when (equal? div-flag #t)
                    (with-primitive ss (blend-mode 'one 'src-color))))
          ((= (osc 0) 2)
              (when (equal? cell-flag #t)
                    (with-primitive s (blend-mode 'one-minus-dst-color 'one-minus-src-color)))
              (when (equal? div-flag #t)
                    (with-primitive ss (blend-mode 'one-minus-dst-color 'one-minus-src-color))))
          ((= (osc 0) 3) 
              (when (equal? cell-flag #t)   
                    (with-primitive s (blend-mode 'one-minus-dst-color 'src-color)))
              (when (equal? div-flag #t)   
                    (with-primitive ss (blend-mode 'one-minus-dst-color 'src-color))))
          ((= (osc 0) 4) 
              (when (equal? cell-flag #t)
                    (with-primitive s (blend-mode 'one 'one)))
              (when (equal? div-flag #t)
                    (with-primitive ss (blend-mode 'one 'one))))
          ((= (osc 0) 5) 
              (when (equal? cell-flag #t)
                    (with-primitive s (blend-mode 'one-minus-dst-color 'zero)))
              (when (equal? div-flag #t)
                    (with-primitive ss (blend-mode 'one-minus-dst-color 'zero))))))
  (when (osc-msg "/speed")
    (set! speed (osc 0)))
  (when (osc-msg "/par")
    (with-primitive p (pdata-map! (lambda (c) (vector 1 1 1 (osc 0))) "c")))
  (when (osc-msg "/max")
    (cond ((= (osc 0) 0) (set! max-flag #f))
          ((= (osc 0) 1) (set! max-flag #t))))
  (when (osc-msg "/str_1")
    (set! str-1 (osc 0)))
  (when (osc-msg "/str_2")
    (set! str-2 (osc 0)))
  (when (osc-msg "/face")
    (cond ((= (osc 0) 0) (set! face-flag #f))
          ((= (osc 0) 1) (set! face-flag #t))))
  (when (osc-msg "/box")
    (cond ((= (osc 0) 0) (set! box-flag #f))
          ((= (osc 0) 1) (set! box-flag #t))))
  (when (osc-msg "/core")
    (cond ((= (osc 0) 0) (set! core-flag #f))
          ((= (osc 0) 1) (set! core-flag #t)
             (set! face-flag #f)
(light-diffuse 0 (vector 0.4 0.4 0.4))
(set! l (make-light 'point 'free))
(light-diffuse l (vector 1 1 1))
(light-position l (vector 20 5 30))
(shadow-light l)
(hint-cast-shadow)
             (destroy male)
             (destroy female)
             (setup pos 0)
               (set! particles (build-list (length active-pos)
                           (lambda (x) (make-object particle% x)))))))
  (when (osc-msg "/echo")
    (cond ((= (osc 0) 0) (blur 1))
	  ((= (osc 0) 1) (blur 0.1))))
  (when (osc-msg "/polygon")
    (cond ((= (osc 0) 0) (set! polygon-flag #f)
			 (for-each (lambda (r) (send r kill)) ribbons)
			 (for-each (lambda (x) (destroy x)) destroy-list)
                         (break-polygon))
          ((= (osc 0) 1) (set! polygon-flag #t)
			 (build-polygon))))
  (when (osc-msg "/seq")
    (with-primitive p
      (pdata-set "p" (round (* size (flxrnd))) (vmul (grndvec) 3))))
  (when (osc-msg "/shrink")
    (when (not (null? shrink-list))
      (let* ((i (random (length shrink-list)))
             (j (list-ref shrink-list i)))
            (send (list-ref particles j) setup)
            (set! shrink-list (remove j shrink-list)))))
  (when (osc-msg "/free")
    (when (and (not ribbon?) (null? shrink-list))
            (set! ribbons (build-list (length active-pos)
                (lambda (x) (make-object ribbon% x))))
            (set! ribbon? #t)))
  (when (osc-msg "/all")
    (destroy-all destroy-list)
    (set! active? #t)
    (setup pos 0)))

; some blend-mode examples
;(blend-mode 'one 'src-color)
;(blend-mode 'one-minus-dst-color 'one-minus-src-color)
;(blend-mode 'one-minus-dst-color 'src-color)
;(blend-mode 'one 'one)
;(blend-mode 'one-minus-dst-color 'zero)

(define (update-camera)
  (set! f (* (- K) (- ps R)))
  (set! as (/ f M))
  (set! vs (* D (+ vs as)))
  (set! ps (+ ps vs))
  (set! theta (- ps R))
  (with-primitive center
    (if (equal? rotate? #t)
        (rotate (vector 1 (if (equal? face-flag #t) 2 1) 0))
        (rotate (vector 0 theta 0))))
    (let ((p (+ 0.5 (* (sin (time)) 0.4)))
          (c (+ 0.2 (* (- (sin (time))) 0.06))))
        (set-camera-transform (mtranslate (vector 0 0 (* -100 c)))))
)

(every-frame
 (begin
   (check-osc)
   (if snake-flag (update-snake speed) 0)
   (if cell-flag (update-cell) 0)
   (if face-flag (update-face) 0)
   (if core-flag (update-core) 0)
   (if polygon-flag (update-polygon) 0)
   (update-camera)))
