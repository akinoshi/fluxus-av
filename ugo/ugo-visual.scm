; File: ugo-visual .scm
; ---------------------
; A script for audio visual performace called 
; UGO (Unidentified Green Object).
;
; SuperCollider: Control Window (Audio)
; Fluxus: Main Window (Visual)
;
; Author: Akinori Kinoshita
; E-mail: art.akinoshi -at- gmail.com
; Date:

(start-audio "SuperCollider:out_1" 256 44100)

(gain 0)

(require scheme/class)

(clear)

; osc setting
(osc-source "12000")
(osc-destination "osc.udp://localhost:57120")

; set up background
(build-image (load-texture "background.png")
  (vector 0 0) (get-screen-size))

; global setting ----------------------------------------------------------
(define line-flag #f)
(define cross-flag #f)
(define particle-flag #f)
(define grid-flag #f)
(define sculpture-flag #f)

(define text-center (build-cube))
(with-primitive text-center (scale 0.001))
(define text1 0)
(define line1 0)
(define text2 0)
(define line2 0)
(define text3 0)
(define line3 0)
(define text4 0)
(define line4 0)
(define text5 0)
(define line5 0)
(define text-cnt 1)

; for debugging -----------------------------------------------------------
(define pos-list '(
#(-2 -2 -2) #(-2  2 -2) #(-2  2  2) #(-2 -2  2)
#( 2  2  2) #( 2 -2  2) #( 2 -2 -2) #( 2  2 -2)
))

; make boundary
#;(for-each (lambda (x)
  (with-state
    (translate x)
    (hint-none)
    (hint-wire)
    (backfacecull 0)
    (build-cube)))
  pos-list)

#;(with-state
  (scale 5)
  (hint-none)
  (hint-wire)
  (backfacecull 0)
  (wire-colour (vector 1 0 0))
  (build-cube))

; part-1 (cube) setting ---------------------------------------------------
(define obj (build-locator))
(define key-debounce #f)

(define height 5)
(define count 0)
(define roll-size 16)
(define i roll-size)
(define pi (* 4.0 (atan 1.0)))
(define (rad->deg rad) (* rad (/ 180 pi)))
(define j 0)
(define step (/ (/ pi 2) roll-size))
(define x (- (* (/ 1 (sqrt 2)) (cos (+ step (/ pi 4)))) 0.5))
(define y (- (* (/ 1 (sqrt 2)) (sin (+ step (/ pi 4)))) 0.5))
(define mode 1)
(define destroy-list '())
(define light? #f)
(define harmonic? #f)
(define step-list '(1 2 4 8 16 32 64))
(define step-index 4)

(define lp (vector -50 50 50))
(define last-time 0)
(define cube-flag #f)

(define cubes '())

; path to get to the cube position
; h: left, j: down, k: up, l: right
; left to right and then snake (right to left)
(define path '(
(k k k k k k k k k k h h)
(k k k k k k k k k k h)
(k k k k k k k k k k)
(k k k k k k k k k k l)
(k k k k k k k k k k l l)
(k k k k k k k k k l l)
(k k k k k k k k k l)
(k k k k k k k k k)
(k k k k k k k k k h)
(k k k k k k k k k h h)
(k k k k k k k k h h)
(k k k k k k k k h)
(k k k k k k k k)
(k k k k k k k k l)
(k k k k k k k k l l)
(k k k k k k k l l)
(k k k k k k k l)
(k k k k k k k)
(k k k k k k k h)
(k k k k k k k h h)
(k k k k k k h h)
(k k k k k k h)
(k k k k k k)
(k k k k k k l)
(k k k k k k l l)
))

(define center (build-cube))
(with-primitive center
  (hide 1))
(lock-camera center)

; rolling cube class
(define cube%
  (class object%
    (init id_)
    (field (id id_))

    (define cnt roll-size)
    (define direction 0)
    (define moving? #f)
    (define rotated? #t)
    (define path-list (list-ref path (modulo id 25)))

    (define obj
      (with-state
        (colour (vector 0 1 0.4))
    (translate (vector 0 (- (floor (/ id 25)) 2) 0))
    (translate (vector 0 0 8))
        (let ((o (build-cube)))
          (set! destroy-list (cons o destroy-list))
          o)))

    (define/public (update)
      (when (and (not (null? path-list)) (not moving?))
        (set! moving? #t)
        (set! rotated? #f)
        (let ((p (list-ref path-list (inexact->exact
                 (floor (* (flxrnd) (length path-list)))))))
          (set! direction p)
          (set! path-list (remove p path-list)))
        (cond
          ((eqv? direction 'h) 0)
          ((eqv? direction 'j)
           (with-primitive obj
             (rotate (vector 0 90 0))))
          ((eqv? direction 'k)
           (with-primitive obj
             (rotate (vector 0 -90 0))))
          ((eqv? direction 'l)
           (with-primitive obj
             (rotate (vector 0 180 0)))))
          (set! cnt 0)))
            
    (define/public (draw)
      (if (>= cnt roll-size)
        (when (not rotated?)
          (set! rotated? #t)
          (with-primitive obj (cond
            ((eqv? direction 'h)
             (rotate (vector 0 0 -90)))
            ((eqv? direction 'j)
             (rotate (vector -90 0 -90)))
            ((eqv? direction 'k)
             (rotate (vector 0 90 0))
             (rotate (vector 90 0 0)))
            ((eqv? direction 'l)
             (rotate (vector 0 0 -90))
             (rotate (vector 0 180 0)))))
             (set! moving? #f)
             (osc-send "/tincan" "f" (list 1)))
            (begin
              (set! cnt (+ cnt 1))
              (with-primitive obj
              (translate (vector x y 0))
              (rotate (vector 0  0 (rad->deg step)))))))

    (define/public (return-obj) obj)
 
    ; check if cube is no more moving
    (define/public (dead?)
      (if (and (null? path-list) (not moving?)) #t #f))

    (super-new)))

(define (destroy-all)
    (destroy-all-rec destroy-list))

(define (destroy-all-rec lst)
    (when (not (null? lst))
        (destroy (car lst))
        (destroy-all-rec (cdr lst))))

(define (update-cube)
  (when cube-flag
    (for-each (lambda (p) (send p update)) cubes)
    (for-each (lambda (p) (send p draw)) cubes)
    (when (and (> (- (time) last-time) 1) (not (>= j (*  5 (length path)))) )
    (set! last-time (time))
          (set! cubes (cons (make-object cube% j) cubes))
          (set! j (+ j 1)))

; test
(when (> j 24)
  (when (and (send (list-ref (reverse cubes) 24) dead?) (= text-cnt 1))
    (setup-text text-cnt)
    (set! text-cnt (+ text-cnt 1))))

(when (> j 49)
  (when (and (send (list-ref (reverse cubes) 49) dead?) (= text-cnt 2))
    (setup-text text-cnt)
    (set! text-cnt (+ text-cnt 1))))

(when (> j 74)
  (when (and (send (list-ref (reverse cubes) 74) dead?) (= text-cnt 3))
    (setup-text text-cnt)
    (set! text-cnt (+ text-cnt 1))))

(when (> j 99)
  (when (and (send (list-ref (reverse cubes) 99) dead?) (= text-cnt 4))
    (setup-text text-cnt)
    (set! text-cnt (+ text-cnt 1))))

(when (> j 124)
  (when (and (send (list-ref (reverse cubes) 124) dead?) (= text-cnt 5))
    (setup-text text-cnt)
    (set! text-cnt (+ text-cnt 1))))

)

#;(when (not (null? cubes))
(with-primitive line1
  (pdata-set! "p" 1 (vtransform (vector 0 0 0) (with-primitive (send (car cubes) return-obj) (get-transform)))))
)

)

(define (setup-text n)
  (cond
    ((= n 1)
     (set! text1 (build-type "Bitstream-Vera-Sans-Mono.ttf" "BASEMENT"))
     (with-primitive text1
     (parent text-center)
     (hint-unlit)
     (colour (vector 1 1 1))
     (translate (vector -3500 0 6000))
     (scale (/ 1000 25)))

     (set! line1 (build-ribbon 3))
     (with-primitive line1
       (parent text-center)
       (hint-wire)
       (hint-unlit)
       (line-width 2)
       (wire-colour (vector 1 1 1))
       (pdata-set! "p" 0 (vector -1000 -1000 6000))
       (pdata-set! "p" 1 (vector -3100 -1000 6000))
       ;(pdata-set! "p" 2 (vtransform (vector 0 0 0) (with-primitive text1 (get-transform))))
       (pdata-set! "p" 2 (vadd (vtransform (vector 0 0 0) (with-primitive text1 (get-transform))) (vector 400 -100 0)))))
    ((= n 2)
     (set! text2 (build-type "Bitstream-Vera-Sans-Mono.ttf" "LIVING ROOM"))
     (with-primitive text2
       (parent text-center)
       (hint-unlit)
       (colour (vector 1 1 1))
       (translate (vector 2500 500 6000))
       (scale (/ 1000 25)))

     (set! line2 (build-ribbon 3))
     (with-primitive line2
       (parent text-center)
       (hint-wire)
       (hint-unlit)
       (line-width 2)
       (wire-colour (vector 1 1 1))
       (pdata-set! "p" 0 (vector 1000 -500 6000))
       (pdata-set! "p" 1 (vector 3100 -500 6000))
       ;(pdata-set! "p" 2 (vtransform (vector 0 0 0) (with-primitive text1 (get-transform))))
       (pdata-set! "p" 2 (vadd (vtransform (vector 0 0 0) (with-primitive text2 (get-transform))) (vector 600 -100 0)))))
    ((= n 3)
     (set! text3 (build-type "Bitstream-Vera-Sans-Mono.ttf" "OFFICE"))
     (with-primitive text3
       (parent text-center)
       (hint-unlit)
       (colour (vector 1 1 1))
       (translate (vector -2500 1000 6000))
       (scale (/ 1000 25)))
     
     (set! line3 (build-ribbon 3))
     (with-primitive line3
       (parent text-center)
       (hint-wire)
       (hint-unlit)
       (line-width 2)
       (wire-colour (vector 1 1 1))
       (pdata-set! "p" 0 (vector -1000 0 6000))
       (pdata-set! "p" 1 (vector -2200 0 6000))
       ;(pdata-set! "p" 2 (vtransform (vector 0 0 0) (with-primitive text1 (get-transform))))
       (pdata-set! "p" 2 (vadd (vtransform (vector 0 0 0) (with-primitive text3 (get-transform))) (vector 300 -100 0)))))
    ((= n 4)
     (set! text4 (build-type "Bitstream-Vera-Sans-Mono.ttf" "KITCHEN"))
     (with-primitive text4
       (parent text-center)
       (hint-unlit)
       (colour (vector 1 1 1))
       (translate (vector 2000 1500 6000))
       (scale (/ 1000 25)))
     
     (set! line4 (build-ribbon 3))
     (with-primitive line4
       (parent text-center)
       (hint-wire)
       (hint-unlit)
       (line-width 2)
       (wire-colour (vector 1 1 1))
       (pdata-set! "p" 0 (vector 1000 500 6000))
       (pdata-set! "p" 1 (vector 2300 500 6000))
       (pdata-set! "p" 2 (vadd (vtransform (vector 0 0 0) (with-primitive text4 (get-transform))) (vector 300 -100 0)))))
    ((= n 5)
     (set! text5 (build-type "Bitstream-Vera-Sans-Mono.ttf" "ROOF"))
     (with-primitive text5
       (parent text-center)
       (hint-unlit)
       (colour (vector 1 1 1))
       (translate (vector -200 2200 6000))
       (scale (/ 1000 25)))
     
     (set! line5 (build-ribbon 2))
     (with-primitive line5
       (parent text-center)
       (hint-wire)
       (hint-unlit)
       (line-width 2)
       (wire-colour (vector 1 1 1))
       (pdata-set! "p" 0 (vector 0 1000 6000))
       (pdata-set! "p" 1 (vadd (vtransform (vector 0 0 0) (with-primitive text5 (get-transform))) (vector 200 -100 0)))))))
  

; part-2 (generative line) setting ----------------------------------------
(collisions 1)
(gravity (vector 0 0 0))
(surface-params 0.4 0 3 5)
(set-physics-debug #f)

(define t1 0)
(define t2 0)
(define t3 0)
(define d 2)
;(define last-time 0)
(define interval 0.2)

(define foo 0)
(define p1 0)
(define p2 0)
(define p3 0)
(define line 0)

#;(define foo (with-state
    (scale 5)
    ;(hint-none)
    (hint-depth-sort)
;    (hint-wire)
    (backfacecull 0)
    (wire-colour (vector 1 0.1 0))
    (colour (vector 0 1 0.4))
    (build-cube)))

(define (build-world size)
  (ground-plane (vector  0  1  0) (- (* 0.5 size)))
  (ground-plane (vector  0 -1  0) (- (* 0.5 size)))
  (ground-plane (vector -1  0  0) (- (* 0.5 size)))
  (ground-plane (vector  1  0  0) (- (* 0.5 size)))
  (ground-plane (vector  0  0  1) (- (* 0.5 size)))
  (ground-plane (vector  0  0 -1) (- (* 0.5 size))))

(define (make-sphere v)
  (with-state
    (hint-none)
    (colour v)
    (scale 0.1)
    (let ((obj (build-sphere 12 12)))
         (active-sphere obj)
         obj)))

;(build-world 5)

;(define p1 (make-sphere (vector 1 0 0)))
;(define p2 (make-sphere (vector 0 1 0)))
;(define p3 (make-sphere (vector 0 0 1)))

;(define line (build-ribbon 3))

#;(with-primitive line
    (hint-none)
    (hint-unlit)
    (hint-wire)
    (wire-colour (vector 1 0 0)))

(define (setup-line)
  (set! foo (with-state
    (scale 5)
    (hint-depth-sort)
    (backfacecull 0)
    (colour (vector 0 1 0.4))
    (build-cube)))
  ; make boundary
  (build-world 5)
  ; setup 3 points for line
  (set! p1 (make-sphere (vector 1 0 0)))
  (set! p2 (make-sphere (vector 0 1 0)))
  (set! p3 (make-sphere (vector 0 0 1)))
  ; set up generative red line
  (set! line (build-ribbon 3))
  (with-primitive line
    (hint-none)
    (hint-unlit)
    (hint-wire)
    (wire-colour (vector 1 0 0))))

(define (update-line)
    (with-primitive p1
        (set! t1 (get-transform)))
    (with-primitive p2
        (set! t2 (get-transform)))
    (with-primitive p3
        (set! t3 (get-transform)))
(when (> (- (time) last-time) interval)
    (let ((temp (build-ribbon 3)))

(set! destroy-list (cons temp destroy-list))

        (with-primitive temp
            (hint-none)
            (hint-unlit)
            (hint-wire)
            (wire-colour (vector 1 0 0 0.2))
            ;(wire-colour (hsv->rgb (vector (abs (sin (time))) 1 0.1)))
            (pdata-set! "p" 0 (vtransform (vector 0 0 0) t1))
            (pdata-set! "p" 1 (vtransform (vector 0 0 0) t2))
            (pdata-set! "p" 2 (vtransform (vector 0 0 0) t3))))
    
    (set! last-time (time)))
    (with-primitive line
        (pdata-set! "p" 0 (vtransform (vector 0 0 0) t1))
        (pdata-set! "p" 1 (vtransform (vector 0 0 0) t2))
        (pdata-set! "p" 2 (vtransform (vector 0 0 0) t3)))
    (when (mouse-button 1)
        (kick p1 (vmul (grndvec) 0.5)))
    (with-primitive foo
        (colour (vector 0 1 0.4 (abs (sin (time))))))
)

; part-3 (cross section) setting ------------------------------------------
(define noise (flxrnd))
(define bottom-surface 0)
(define bottom-left 0)
(define bottom-front 0)
(define bottom-right 0)
(define bottom-back 0)
(define bottom-bottom 0)
(define top-surface 0)
(define top-left 0)
(define top-front 0)
(define top-right 0)
(define top-back 0)
(define top-bottom 0)

(define ring-id 0)
(define ring-flag #f)
(define p (build-particles 300))
(define particle-pos (build-list 300 (lambda (x) (grndvec))))

(with-primitive p
  (pdata-map! (lambda (p) (vector 1000 0 0)) "p"))

(define (gl n)
  (log (+ 1 (gh n))))

(define (setup-cross)

; bottom-surface
(push)
#;(shader "blinn.vert.glsl"
        "blinn.frag.glsl")
(set! bottom-surface
  (with-state
    (translate (vector 0 0 0))
    (colour (vector 1 0 0))
    (rotate (vector -90 0 0))
    (scale 5)
    (build-nurbs-plane 16 16)))
(pop)

#;(grab bottom-surface)
(shader-set! (list 
  "LightPos" (vector 0 50 50)
  "AmbietColour" (vector 0.1 0.1 0.1)
  "DiffuseColour" (vector 1 0 0)
  "SpecularColour" (vector 1 1 0)
  "Roughness" 0.05
  "AmbientIntensity" 1.0
  "DiffuseIntensity" 1.0
  "SpecularIntensity" 1.0
  "Sharpness" 0.6))
(ungrab)

; bottom left
(set! bottom-left
  (with-state
    (parent bottom-surface)
    (colour (vector 0 1 0.4))
    (rotate (vector 0 -90 0))
    (translate (vector -0.5 0 0.5))
    (scale (vector 0.001 1 1))
    (build-nurbs-plane 16 16)))

; bottom front
(set! bottom-front
  (with-state
    (parent bottom-surface)
    (colour (vector 0 1 0.4))
    (rotate (vector 90 0 0))
    (translate (vector 0 -0.5 0.5))
    (scale (vector 1 0.001 1))
    (build-nurbs-plane 16 16)))

; bottom right
(set! bottom-right
  (with-state
    (parent bottom-surface)
    (colour (vector 0 1 0.4))
    (rotate (vector 0 90 0))
    (translate (vector 0.5 0 0.5))
    (scale (vector 0.001 1 1))
    (build-nurbs-plane 16 16)))

; bottom back
(set! bottom-back
  (with-state
    (parent bottom-surface)
    (colour (vector 0 1 0.4))
    (rotate (vector -90 0 0))
    (translate (vector 0 0.5 0.5))
    (scale (vector 1 0.001 1))
    (build-nurbs-plane 16 16)))

; bottom bottom
(set! bottom-bottom
  (with-state
    (parent bottom-surface)
    (colour (vector 0 1 0.4))
    (translate (vector 0 0 -0.5))
    (build-plane)))

; top-surface
(push)
(shader "blinn.vert.glsl"
        "blinn.frag.glsl")
(set! top-surface
  (with-state
    ;(hint-origin)
    ;(backfacecull 0)
    ;(hint-unlit)
    (translate (vector 0 0 0))
    ;(backfacecull 0)
    (colour (vector 1 0 0))
    (rotate (vector 90 0 0))
    (scale 5)
    (build-nurbs-plane 16 16)))
(pop)

(grab top-surface)
(shader-set! (list 
  "LightPos" (vector 0 50 50)
  "AmbietColour" (vector 0.1 0.1 0.1)
  "DiffuseColour" (vector 1 0 0)
  "SpecularColour" (vector 1 1 0)
  "Roughness" 0.05
  "AmbientIntensity" 1.0
  "DiffuseIntensity" 1.0
  "SpecularIntensity" 1.0
  "Sharpness" 0.6))
(ungrab)

; top left
(set! top-left
  (with-state
    (parent top-surface)
    (colour (vector 0 1 0.4))
    (rotate (vector 0 -90 0))
    (translate (vector -0.5 0 0.5))
    (scale (vector 0.001 1 1))
    (build-nurbs-plane 16 16)))

; top front
(set! top-front
  (with-state
    (parent top-surface)
    (colour (vector 0 1 0.4))
    (rotate (vector -90 0 0))
    (translate (vector 0 0.5 0.5))
    (scale (vector 1 0.001 1))
    (build-nurbs-plane 16 16)))

; top right
(set! top-right
  (with-state
    (parent top-surface)
    (colour (vector 0 1 0.4))
    (rotate (vector 0 90 0))
    (translate (vector 0.5 0 0.5))
    (scale (vector 0.001 1 1))
    (build-nurbs-plane 16 16)))

; top back
(set! top-back
  (with-state
    (parent top-surface)
    (colour (vector 0 1 0.4))
    (rotate (vector 90 0 0))
    (translate (vector 0 -0.5 0.5))
    (scale (vector 1 0.001 1))
    (build-nurbs-plane 16 16)))

; top bottom
(set! top-bottom
  (with-state
    (parent top-surface)
    (colour (vector 0 1 0.4))
    (translate (vector 0 0 -0.5))
    (build-plane)))

)

(define (update-cross)
  (with-primitive bottom-surface
    (recalc-normals 1)
    (pdata-index-map! (lambda (i p)
      (if (= i -1)
        (vector 0 0 0)
        (vector (vx p) (vy p)
                (* 0.8 (/ (abs (cos (time))) 2) (sin (* noise (+ (time) i))))
                ;(* 0.15 (sin (* noise i)))
        )))
    "p"))

  (with-primitive bottom-left
    (recalc-normals 1)
    (for-each (lambda (i j)
      (let ((temp
        (with-primitive bottom-surface (pdata-ref "p" j))))
        (pdata-set! "p" i (vector (+ 500 (* 1000 (vz temp)))
                                  (vy temp)
                                  (+ 0.5 (vx temp))))))
      (build-list 17 (lambda (x) (- 288 x)))
      (build-list 17 (lambda (y) (- 16 y)))))

  (with-primitive bottom-front
    (recalc-normals 1)
    (for-each (lambda (i j)
      (let ((temp
        (with-primitive bottom-surface (pdata-ref "p" j))))
        (pdata-set! "p" i (vector (vx temp)
                                  (- (* 1000 (vz temp)) -500)
                                  (+ 0.5 (vy temp))))))
      (build-list 17 (lambda (x) (+ (* 16 (+ x 1)) x)))
      (build-list 17 (lambda (y) (+ (* 16 y) y)))))

  (with-primitive bottom-right
    ;(recalc-normals 1)
    (for-each (lambda (i j)
      (let ((temp
        (with-primitive bottom-surface (pdata-ref "p" j))))
        (pdata-set! "p" i (vector (- (* -1000 (vz temp)) 500)
                                  (vy temp)
                                  (- 0.5 (vx temp))))))
      ;(build-list 17 (lambda (x) (+ (* 16 (+ x 1)) x)))
      (build-list 17 (lambda (x) x))
      (build-list 17 (lambda (y) (+ 272 y)))))

  (with-primitive bottom-back
    (recalc-normals 1)
    (for-each (lambda (i j)
      (let ((temp
        (with-primitive bottom-surface (pdata-ref "p" j))))
        (pdata-set! "p" i (vector (vx temp)
                                  (- (* -1000 (vz temp)) 500)
                                  (- 0.5 (vy temp))))))
      (build-list 17 (lambda (y) (+ (* 16 y) y)))
      (build-list 17 (lambda (y) (+ (* 16 (+ y 1)) y)))))

    (with-primitive top-back
        (recalc-normals 1)
        (pdata-index-map! (lambda (i p)
            (if (= i -1)
                (vector 0 0 0)
                (vector (vx p) (vy p) (vz p))))
            "p"))

#;  (with-primitive top-back
    (recalc-normals 1)
    (pdata-index-map! (lambda (i p)
      (if (= i -1)
        (vector 0 0 0)
        (vector (vx p) (vy p)
                ;(* 0.05 (sin (* (/ (mouse-y) 800) (+ (time) i))))
                (* 0.15 (sin (* noise i)))
        )))
    "p"))

  (with-primitive top-surface
    (recalc-normals 1)
    (for-each (lambda (i j)
      (let ((temp
        (with-primitive top-surface (pdata-ref "p" j))))
        (pdata-set! "p" j (vector (vx temp)
                                  (vy temp)
                                  (* -0.8 (/ (abs (cos (time))) 2) (sin (* noise (+ (time) i))))
                                  ;(* -1 (* 0.15 (sin (* noise i))) ) 
))))
        (build-list 289 (lambda (x) x))
    (build-list 289 (lambda (y) (- (+ (* 16 (+ (floor (/ y 17)) 1))
                      (floor (/ y 17)))
                       (modulo y 17))))))

  (with-primitive top-left
    (recalc-normals 1)
    (for-each (lambda (i j)
      (let ((temp
        (with-primitive top-surface (pdata-ref "p" j))))
        (pdata-set! "p" i (vector (+ 500 (* 1000 (vz temp)))
                                  (vy temp)
                                  (+ 0.5 (vx temp))))))
      (build-list 17 (lambda (x) (- 288 x)))
      (build-list 17 (lambda (y) (- 16 y)))))

  (with-primitive top-front
    (recalc-normals 1)
    (for-each (lambda (i j)
      (let ((temp
        (with-primitive top-surface (pdata-ref "p" j))))
        (pdata-set! "p" i (vector (vx temp)
                                  (- (* -1000 (vz temp)) 500)
                                  (- 0.5 (vy temp))))))
      (build-list 17 (lambda (x) (+ (* 16 x) x)))
      (build-list 17 (lambda (y) (+ (* 16 (+ y 1)) y)))))

  (with-primitive top-right
    ;(recalc-normals 1)
    (for-each (lambda (i j)
      (let ((temp
        (with-primitive top-surface (pdata-ref "p" j))))
        (pdata-set! "p" i (vector (- (* -1000 (vz temp)) 500)
                                  (vy temp)
                                  (- 0.5 (vx temp))))))
      ;(build-list 17 (lambda (x) (+ (* 16 (+ x 1)) x)))
      (build-list 17 (lambda (x) x))
      (build-list 17 (lambda (y) (+ 272 y)))))

  (with-primitive top-back
    (recalc-normals 1)
    (for-each (lambda (i j)
      (let ((temp
        (with-primitive top-surface (pdata-ref "p" j))))
        (pdata-set! "p" i (vector (vx temp)
                                  (- (* 1000 (vz temp)) -500)
                                  (+ 0.5 (vy temp))))))
      (build-list 17 (lambda (x) (+ (* 16 (+ x 1)) x)))
      (build-list 17 (lambda (y) (+ (* 16 y) y)))))


    (with-primitive bottom-surface
        (identity)
(rotate (vector 90 0 0))
;(rotate (vector 0 0 (* 45 (gl 0))))
(rotate (vector (* -45 (gl 1)) (* -30 (gl 2)) 0))
        ;(translate (vector 0 0 (+ 1 (* -1 (/ (mouse-y) 800)))))
        (translate (vector 0 0 (* -2 (gl 0))))
        (scale 5)
        (rotate (vector 0 0 (* 0 (cos (time)))))
        (rotate (vector 0 0 (* 0 (gh 0)))))
    (with-primitive top-surface
        (identity)
(rotate (vector 90 0 0))
;(rotate (vector 0 0 (* -45 (gl 0))))
(rotate (vector (* -45 (gl 1)) (* -30 (gl 2)) 0))
        ;(translate (vector 0 0 (+ 1 (* 1 (/ (mouse-y) 800)))))
        (translate (vector 0 0 (* 2 (gl 0))))
        (rotate (vector 180 0 0))
        (rotate (vector 0 0 (* 0 (cos (time)))))
        (scale 5))

(when (not (= ring-id 0))
  (with-primitive ring-id
    (identity)
    (rotate (vector 90 0 0))
    (rotate (vector (* -45 (gl 1)) (* -30 (gl 2)) 0))))

(when particle-flag
  (with-primitive p
    (identity)
    (scale 5)
    (rotate (vector 90 0 0))
    (rotate (vector (* -45 (gl 1)) (* -30 (gl 2)) 0))))

)

(define (build-ring)
  (when (not (= ring-id 0))
    (destroy ring-id))
  (set! noise (flxrnd))
  (set! ring-id (build-torus 0.15 (+ 4 (* 6 noise)) 48 48))
  (with-primitive ring-id
    (shader "gooch.vert.glsl" "gooch.frag.glsl")
    (shader-set!
    (list "LightPos" (vector -50 50 50)
          "DiffuseColour" (vector 1 1 1)
          "WarmColour" (vector 0.6 0.6 0)
          "CoolColour" (vector 0 0 0.6)
          "OutlineWidth" 0.4))
    (identity)
    (rotate (vector 90 0 0))
    (rotate (vector (* -45 (gl 1)) (* -30 (gl 2)) 0))))

(define (update-particle)
  (with-primitive p
    (pdata-index-map! (lambda (i p) (vadd p (vdiv (vsub (list-ref particle-pos i) p) 500))) "p")))

; part-4 (grid) setting ---------------------------------------------------
(define pos-list '(
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

;(define radius 1) ; 10
(define radius 0.01) ; 10
(define damping 0.05)
(define grids 0)

;(define pi 3.141592653589793238462643)
(define 2pi (* 2 pi))
(define n 554)
(define snake-fade 0)
(define speed 1)
(define max-cnt 100)
(define cnt 69) ; 0
(define snake 0)
(define snake-flag #t)

(define ball 0)
(define ball-scale 0)
(define followers '())
(define follower-id 0)
(define amp-list '())
(define width-list '())

(define stop? #f)

(define (setup-grid)

(set! ball (build-sphere 24 24))

(with-primitive ball
  (shader "facingratio.vert.glsl" "facingratio.frag.glsl")
  (shader-set! (list "InnerColour" (vector (/ 231 255) (/ 84 255) (/ 15 255) 1)
                     "OuterColour" (vector (/ 244 255) (/ 243 255) (/ 90 255) 1))))

(with-primitive ball
  (colour (vector 1 0 0))
  (scale 0.1))
)

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

(define dst-path (build-list n
  (lambda (i) (vector 0 0 0))))

(define (get-new-path src-list dst-list)
  (map (lambda (x y) (vadd (vdiv (vsub y x) max-cnt) x)) src-list dst-list))

(define f-path knot-3)

(define grid%
    (class object%
        (init id_)

        (field [id id_])

        (define pos #(0 0 0))    
        (define vel #(0 0 0))
        (define acc #(0 0 0))
        (define dumping 0.05)

        (set! pos (list-ref pos-list id_))

(define cube (with-state
;    (hint-unlit)
;    (backfacecull 0)
;    (hint-cull-ccw)
    ;(opacity 0.2)
    ;(colour (vector 0 1 0.4))
    (if (= id 119)
      (colour (vector 1 0 0))
      (colour (vector 0 1 0.4)))
    (translate pos)
;    (hint-depth-sort)
    (build-cube)))

        (define/public (reset-force)
            (set! acc (vector 0 0 0)))

        (define/public (add-rep-force x y z r s)
            (let* ((diff (vsub pos (vector x y z)))
                   (len (sqrt (+ (* (vx diff) (vx diff))
                                 (* (vy diff) (vy diff))
                                 (* (vz diff) (vz diff)))))
                   (close? (if (> len radius) #f #t))
                   (ptc (- 1 (/ len radius)))
                   (diff (vector (/ (vx diff) len)
                                 (/ (vy diff) len)
                                 (/ (vz diff) len))))
            (when close?
                (set! acc (vector (+ (vx acc) (* (vx diff) 0.5 ptc))
                                  (+ (vy acc) (* (vy diff) 0.5 ptc))
                                  (+ (vz acc) (* (vz diff) 0.5 ptc)))))))

        (define/public (add-damping-force)
            (set! acc (vector (- (vx acc) (* (vx vel) damping))
                              (- (vy acc) (* (vy vel) damping))
                              (- (vz acc) (* (vz vel) damping)))))

#;        (define/public (add-force x y z)
            (set! acc (vector (+ (vx acc) x)
                              (+ (vy acc) y)
                              (+ (vz acc) z))))

        (define/public (add-force v)
            (set! acc (vector (+ (vx acc) (vx v))
                              (+ (vy acc) (vy v))
                              (+ (vz acc) (vz v)))))

        (define/public (update)
            (set! vel (vadd vel acc))
            (set! pos (vadd pos vel))

#;(when (= id 0)
  (osc-send "/grid" "f" (list (vdist pos (list-ref pos-list id))))
  (display (vdist pos (list-ref pos-list id)))
  (newline))

(with-primitive cube
    ;(when sculpture-flag (opacity (gh 5)))
    (translate vel))

            (with-state
#;(let* ((dist (vdist pos #(0 0 0)))
       (size (/ (max 0 (- 3 dist)) 1)))
    (scale size))
                (hint-unlit)
                ;(hint-wire)
                (backfacecull 0)
                (colour #(1 1 1 0.1))
                ;(blend-mode 'one 'one)
                (translate pos)
                ;(translate (list-ref pos-list id_))
;           (hint-depth-sort)
;                (draw-cube)
))

        (define/public (get-pos) pos)
        (define/public (get-vel) vel)

        (super-new)))

(define follower%
  (class object%
    (init id_)
    (field [id id_])

    (define f (build-ribbon 100))
    (define pos (vmul (grndvec) 5))
    (define d (vmul (grndvec) 2))
    (define count 0)
    (define finish? #f)
    (define dead? #f)

    (with-primitive f
      (hint-unlit)
      (hint-vertcols)
      (pdata-map! (lambda (w) 0.1) "w")
      (pdata-map! (lambda (c) (vector 0 0 0)) "c")
      (pdata-map! (lambda (p) pos) "p"))

    (define/public (render)
      (with-primitive f
        ;(when (> (+ count 1) (- (pdata-size) 1)) (set! finish? #t))
        (when (> (+ count 1) (- (pdata-size) 1)) 
          (if finish?
                  (set! dead? #t)
          (set! finish? #t)))
        (set! count (modulo (+ count 1) (pdata-size)))
        (let* ((x (* 0.9 (- (vx d) (/ (- (vx pos)
               (vx (list-ref f-path cnt)) ) 20))))
               (y (* 0.9 (- (vy d) (/ (- (vy pos)
               (vy (list-ref f-path cnt)) ) 20))))
               (z (* 0.9 (- (vz d) (/ (- (vz pos)
               (vz (list-ref f-path cnt)) ) 20)))))
              (set! d (vector x y z))
        (when (not finish?)
          (pdata-set! "p" count (vector (+ (vx pos) x)
                                        (+ (vy pos) y)
                                        (+ (vz pos) z))))
          (set! pos (pdata-ref "p" count)))
        (when (not finish?)
          (pdata-index-map! (lambda (i p) (if (> i count) pos (pdata-ref "p" i))) "p"))
        (when finish?
          (pdata-index-map! (lambda (i p) (if (< i count) pos (pdata-ref "p" i))) "p"))))

    (define/public (get-finish) finish?)  
    (define/public (get-dead) dead?)  
    (define/public (kill) (destroy f))

    (super-new)))

(define (send-grid)
  (osc-send "/grid" "ffff"
    (list (vy (send (list-ref grids 119) get-pos))
          (vdist (list-ref pos-list 119) (send (list-ref grids 119) get-pos))
          (vy (send (list-ref grids 119) get-vel))
          (vx (send (list-ref grids 119) get-pos)))))

(define (update-grid)

  (send-grid)

    (for-each
        (lambda (x)
            (send x reset-force))
        grids)
    (for-each
        (lambda (x i)
            (send x add-force (vdiv (vsub (list-ref pos-list i)
                                          (send x get-pos))
                                    10)))
        grids
        (build-list (length grids) (lambda (x) x)))

  ;(for-each (lambda (x) (send x add-rep-force 0 0 0 0 0)) grids)
  ;(for-each (lambda (x) (send x add-rep-force (* 5 (sin (* 2 (time)))) 0 0 0 0)) grids)
  (let ((temp (list-ref f-path cnt)))
    (for-each (lambda (x) (send x add-rep-force (vx temp) (vy temp) (vz temp) 0 0)) grids))

#;(for-each (lambda (x) (let ((foo (with-primitive s (pdata-ref "p" 181))))
    (send x add-rep-force (* 1 (vx foo)) (* 1 (vy foo)) (* 1 (vz foo)) 0 0)))
    grids)

#;(for-each (lambda (x) (let ((foo (with-primitive s (pdata-ref "p" (+ 181 20)))))
    (send x add-rep-force (* 1 (vx foo)) (* 1 (vy foo)) (* 1 (vz foo)) 0 0)))
    grids)

    (for-each
        (lambda (x)
            (send x add-damping-force))
        grids)
    (for-each
        (lambda (x)
            (send x update))
        grids))

(define (update-force)
  (when stop? (set! f-path (get-new-path f-path dst-path)))
  (set! cnt (+ cnt 1))
  (set! cnt (modulo cnt n))
  ;(display cnt)
  ;(newline)
  #;(with-primitive ball
    (identity)
    (translate (list-ref f-path cnt))
    (let ((temp (+ (* (floor (/ follower-id 5)) 0.1) 0.1)))
         (if (>= temp 1)
           (set! ball-scale 1) 
             (set! ball-scale temp)))
    (scale ball-scale)
    (set! radius (* ball-scale 10)))
  (with-primitive ball
    (identity)
    (translate (list-ref f-path cnt))
    (scale (/ radius 10)))
)

(define (update-follower)

  (when (osc-msg "/melody") (when (= (osc 0) 3)
   (set! followers (cons (make-object follower% follower-id) followers))
   (set! follower-id (+ follower-id 1))))

  (for-each (lambda (x) (send x render)) followers)
  ; destroy follower if it's finished
  (when (not (null? followers))
    (when (send (car followers) get-dead) 
      (send (car followers) kill)
      (set! followers (cdr followers))))

)

; part-5 (sculpture) setting ----------------------------------------------
(define s 0)
(define s-cnt 0)
(define gh-cnt 0)
(define clock-list '())
(define clock-cnt 0)

(define clock% (class object%
  (init id_)
  (field (id id_))
        
  ; spring simulation
  (define M 0.2) ; 0.2
  (define K 0.1) ; 0.2
  (define D 0.8) ; 0.2
  (define R 0)
  (define ps 0.0)
  (define vs 0.0)
  (define as 0)
  (define f 0)
        
  ; ellipsoid
  (define n 50)
  (define radius (+ (* 4 (flxrnd)) 1))
  (define target radius)
  (set! R radius)
  (set! radius 0)
  (define path 0)
  (define inner-path 0)
  (define outer-path 0)
  (define ring 0)
        
  (define grid-list (build-list 20 (lambda (x) (build-ribbon 2))))
  (define cnt 0)
        
  (define clock? (if (> (flxrnd) 0.5) #t #f))
        
  ; random rotation
  (define ang (vector (* 360 (flxrnd))
                      (* 360 (flxrnd))
                    (* 360 (flxrnd))))
        
  (for-each (lambda (x)
    (with-primitive x
      (rotate ang)))
      grid-list)
        
  ; initialize path
  (set! path (build-list n (lambda (i)
              (let* ((theta (/ (* i 2pi) n))
                    (phi 0)
                    (x (* radius (cos theta) (cos phi)))
                    (y (* radius (cos theta) (sin phi)))
                    (z (* radius (sin theta))))
                    (vector x y z)))))
        
  ; append the fist position at tail
  (set! path (reverse (cons (car path) (reverse path))))
        
  (set! ring (build-ribbon (+ n 1)))
        
  (with-primitive ring
    (rotate ang)
    (hint-unlit)
    (hint-vertcols)
    (pdata-index-map!
      (lambda (i pos) (list-ref path i)) "p")
        (pdata-map! (lambda (w) 0.01) "w")
        (pdata-index-map!
          (lambda (i c) (hsv->rgb (vector (/ i (+ n 1)) 1 1)))
          "c"))
        
  (define/public (update)
    ; fake update
    ; (set! radius (* target (sin (time))))
    (set! f (* (- K) (- ps R)))
    (set! as (/ f M))
    (set! vs (* D (+ vs as)))
    (set! ps (+ ps vs))
    (set! radius (+ ps R))
    (set! path (build-list n (lambda (i)
      (let* ((theta (/ (* i 2pi) n))
        (phi 0)
        (x (* radius (cos theta) (cos phi)))
        (y (* radius (cos theta) (sin phi)))
        (z (* radius (sin theta))))
        (vector x y z)))))
            
    (set! path (reverse (cons (car path) (reverse path))))
    ;---------------------------------------------------------------
    (when clock?
      (set! outer-path (build-list 20
        (lambda (i)
          (let* ((theta (/ (* i 2pi) 20))
            (phi 0)
            (x (* 0.98 radius (cos theta) (cos phi)))
            (y (* 0.98 radius (cos theta) (sin phi)))
            (z (* 0.98 radius (sin theta))))
            (vector x y z)))))
              
      (set! inner-path (build-list 20
        (lambda (i)
          (let* ((theta (/ (* i 2pi) 20))
            (phi 0)
            (x (* 0.93 radius (cos theta) (cos phi)))
            (y (* 0.93 radius (cos theta) (sin phi)))
            (z (* 0.93 radius (sin theta))))
            (vector x y z)))))
                
      (set! cnt 0)
      (for-each
        (lambda (in out lst)
          (with-primitive lst
            (set! cnt (+ cnt 1))
              (pdata-map! (lambda (c) (hsv->rgb (vector (/ cnt (length inner-path)) 1 1))) "c")
              (pdata-map! (lambda (w) 0.01) "w")
              (hint-unlit)
              (hint-vertcols)
              (pdata-set! "p" 0 in)
              (pdata-set! "p" 1 out)))
        inner-path
        outer-path
        grid-list)
      )
      ;---------------------------------------------------------------
      (with-primitive ring
        (pdata-index-map!
          (lambda (i pos) (list-ref path i)) "p")))
        
      (super-new)))

(define deform-list
  ;'(161 181 201)
  '(1 21 41 61 81 101 121 141 161 181
    201 221 241 261 281 301 321 341 361)
)

(define (setup-sculpture)
  (set! s (build-sphere 20 20))

  (with-primitive s
    (shader "facingratio.vert.glsl" "facingratio.frag.glsl")
    (shader-set! (list "InnerColour" (vector (/ 231 255) (/ 84 255) (/ 15 255) 1)
                       "OuterColour" (vector (/ 244 255) (/ 243 255) (/ 90 255) 1)))
    (poly-convert-to-indexed)
    (pdata-copy "p" "pref")
    (pdata-copy "n" "nref")))

(define (animate)
  (set! gh-cnt (round (* 13 (sin (time)))))
  (for-each (lambda (n)
    (pdata-set! "p" n (vadd (pdata-get "pref" n)
                            (vmul (pdata-get "nref" n) (gh gh-cnt))))
    (set! gh-cnt (+ gh-cnt 1)))
    (map (lambda (x) (+ x s-cnt)) deform-list)))

(define (update-sculpture)

  (send-grid)

  (with-primitive s
    (animate)
    (recalc-normals 0))
  (when (< 0.025 (- (time) last-time))
    (set! last-time (time))
    (set! s-cnt (modulo (+ s-cnt 1) 20)))

  (for-each (lambda (x) (send x reset-force)) grids)
  (for-each (lambda (x i)
    (send x add-force (vdiv (vsub (list-ref pos-list i)
                                  (send x get-pos)) 10)))
     grids (build-list (length grids) (lambda (x) x)))

(for-each (lambda (x) (let ((foo (with-primitive s (pdata-ref "p" 181))))
    (send x add-rep-force (* 1 (vx foo)) (* 1 (vy foo)) (* 1 (vz foo)) 0 0)))
    grids)

(for-each (lambda (x) (let ((foo (with-primitive s (pdata-ref "p" (+ 181 20)))))
    (send x add-rep-force (* 1 (vx foo)) (* 1 (vy foo)) (* 1 (vz foo)) 0 0)))
    grids)

  (for-each (lambda (x) (send x add-damping-force)) grids)
  (for-each (lambda (x) (send x update)) grids)

  (for-each (lambda (x) (send x update)) clock-list)

  (when particle-flag (with-primitive p (rotate (vector (gh 0) (gh 1) 0)))))


; key listener
(define (check-key) (cond
  ((key-pressed "1")
     (when (not key-debounce)
     (set! key-debounce #t)
     (set! cube-flag #t)


))
 
  ((key-pressed "2")
   (when (not key-debounce)
   (set! key-debounce #t)
   (set! cube-flag #f)
   (destroy-all)
   (set! destroy-list '())
   (setup-line)
   (destroy text1)
   (destroy line1)
   (destroy text2)
   (destroy line2)
   (destroy text3)
   (destroy line3)
   (destroy text4)
   (destroy line4)
   (destroy text5)
   (destroy line5)
   (set! line-flag #t)))

  ((key-pressed "w")
   (when (not key-debounce)
   (set! key-debounce #t)
   (lock-camera p1)))

  ((key-pressed "s")
   (when (not key-debounce)
   (set! key-debounce #t)
   (lock-camera center)))

  ((key-pressed "3")
   (when (not key-debounce)
   (set! key-debounce #t)
   (set! line-flag #f)
   (destroy foo)
   (destroy line)
   (destroy p1)
   (destroy p2)
   (destroy p3)
   (destroy-all)
   (setup-cross)
   (set! cross-flag #t)))

  ((key-pressed "e")
   (when (not key-debounce)
   (set! key-debounce #t)
   (when (not (= ring-id 0))
     (destroy ring-id))
   (set! noise (flxrnd))
   (set! ring-id (build-torus 0.15 (+ 4 (* 6 noise)) 48 48))
   (with-primitive ring-id
     (shader "gooch.vert.glsl" "gooch.frag.glsl")
     (shader-set!
     (list "LightPos" (vector -50 50 50)
           "DiffuseColour" (vector 1 1 1)
           "WarmColour" (vector 0.6 0.6 0)
           "CoolColour" (vector 0 0 0.6)
           "OutlineWidth" 0.4)))))

  ((key-pressed "d")
   (when (and (not key-debounce) (not particle-flag))
   (set! key-debounce #t)
   (with-primitive p
     (hint-depth-sort)
     (pdata-map! (lambda (c) (vector 1 1 1)) "c")
     (texture (load-texture "particle.png"))
     (pdata-map! (lambda (p) (vector 0 0 0)) "p")
     ;(pdata-map! (lambda (p) (vmul (grndvec) 1)) "p")
     (scale 5))
   (set! particle-flag #t)
))

  ((key-pressed "c")
   (when (not key-debounce)
   (set! key-debounce #t)
   (destroy ring-id)))

  ((key-pressed "4")
   (when (not key-debounce)
   (set! key-debounce #t)
   (set! cross-flag #f)
   (destroy bottom-surface)
   (destroy bottom-left)
   (destroy bottom-front)
   (destroy bottom-right)
   (destroy bottom-back)
   (destroy bottom-bottom)
   (destroy top-surface)
   (destroy top-left)
   (destroy top-front)
   (destroy top-right)
   (destroy top-back)
   (destroy top-bottom)
   (set! grids (build-list (length pos-list)
     (lambda (x) (make-object grid% x))))
   (setup-grid)
   (set! grid-flag #t)))

  ((key-pressed "r")
   (when (not key-debounce)
   (set! key-debounce #t)
   (set! followers (cons (make-object follower% follower-id) followers))
   (set! follower-id (+ follower-id 1))))

  ((key-pressed "f")
   (when (not key-debounce)
   (set! key-debounce #t)
   (set! stop? #t)))

  ((key-pressed "5")
   (when (not key-debounce)
   (set! key-debounce #t)
   (set! grid-flag #f)
   (destroy ball)
   (setup-sculpture)
   (set! radius 10)
   (set! sculpture-flag #t)))

    ((key-pressed "a")
    0
    ;(set! cube-flag #t)
     ;(when (and (not key-debounce) (= mode 1))
     ;(set! key-debounce #t)
     ;(when (not (>= j (*  5 (length path))))
     ;    (set! cubes (cons (make-object cube% j) cubes))
     ;    (set! j (+ j 1)))
)

  ((key-pressed "t")
   (when (not key-debounce)
   (set! key-debounce #t)
   (set! clock-cnt (+ clock-cnt 1))
   (set! clock-list (cons (make-object clock% cnt) clock-list))))

    (else (set! key-debounce #f))))

; scene setup ------------------------------------------------------------
(define (setup-scene-1)
  (set! cube-flag #t))

(define (setup-scene-2)
  (set! cube-flag #f)
  (destroy-all)
  (set! destroy-list '())
  (setup-line)
  (destroy text1)
  (destroy line1)
  (destroy text2)
  (destroy line2)
  (destroy text3)
  (destroy line3)
  (destroy text4)
  (destroy line4)
  (destroy text5)
  (destroy line5)
  (set! line-flag #t))

(define (setup-scene-3)
  (set! line-flag #f)
  (destroy foo)
  (destroy line)
  (destroy p1)
  (destroy p2)
  (destroy p3)
  (destroy-all)
  (setup-cross)
  (set! cross-flag #t))

(define (setup-scene-4)
  (set! cross-flag #f)
  (destroy bottom-surface)
  (destroy bottom-left)
  (destroy bottom-front)
  (destroy bottom-right)
  (destroy bottom-back)
  (destroy bottom-bottom)
  (destroy top-surface)
  (destroy top-left)
  (destroy top-front)
  (destroy top-right)
  (destroy top-back)
  (destroy top-bottom)
  (set! grids (build-list (length pos-list)
    (lambda (x) (make-object grid% x))))
  (setup-grid)
  (set! grid-flag #t))

(define (setup-scene-5)
  (set! grid-flag #f)
  (destroy ball)
  (setup-sculpture)
  (set! radius 10)
  (set! sculpture-flag #t))

(define (setup-particle)
  (when (not particle-flag)
    (with-primitive p
      (hint-depth-sort)
       (pdata-map! (lambda (c) (vector 1 1 1)) "c")
       (texture (load-texture "particle.png"))
       (pdata-map! (lambda (p) (vector 0 0 0)) "p")
       (scale 5))
    (set! particle-flag #t)))

; control setting --------------------------------------------------------
(define (check-osc)
  (when (osc-msg "/scene")
    (cond ((= (osc 0) 1) (setup-scene-1))
          ((= (osc 0) 2) (setup-scene-2))
          ((= (osc 0) 3) (setup-scene-3))
          ((= (osc 0) 4) (setup-scene-4))
          ((= (osc 0) 5) (setup-scene-5))))
  (when (osc-msg "/lock")
    (cond ((= (osc 0) 0) (lock-camera center))
          ((= (osc 0) 1) (lock-camera p1))))
  (when (osc-msg "/ring") (build-ring))
  (when (osc-msg "/gain") (gain (osc 0)))
  (when (osc-msg "/kill") (destroy ring-id))
  (when (osc-msg "/particle") (setup-particle))
  (when (osc-msg "/size") (set! radius (* (osc 0) 10))))

(every-frame
  (begin
    (check-key)
    (check-osc)
    (when cube-flag (update-cube))
    (when line-flag (update-line))
    (when cross-flag (update-cross))
    (when particle-flag (update-particle))
    (when grid-flag (update-grid)
                    (update-force)
                    (update-follower))
    (when sculpture-flag (update-sculpture))
    (if sculpture-flag
      (with-primitive center (rotate (vector (sin (time)) (cos (time)) 0)))
      (if cross-flag
        (with-primitive center (rotate (vector 0 -4 0))) ; 0 -2.75 0
        (with-primitive center (rotate (vector 0 -1 0)))))

    (with-primitive text-center
      (rotate (vector 0 -1 0)))
))
