#lang racket
(require 2htdp/image 2htdp/universe)

#| A purely-functional, very-slow but very-fun Ray-Caster in Racket.
    Many thanks to http://www.permadi.com/tutorial/raycast/

  Possible improvements:
         - Typed-racket conversion!
         - Textures
|#

; x & y are coordinates (floats)
(struct posn (x y) #:transparent)

;size of each block in the map
(define GRID-SIZE 64)

;pos is our current position
(define pos (posn (* 17 64) (* 2 64)))

;our current viewing-angle (in radians!)
(define viewing-angle (degrees->radians 270))

;walking speed
(define SPEED 6)

;Field Of View is specified in degrees, but immediately converted to radians
(define FOV (degrees->radians 60))

;1.5 seconds (assuming 30 fps) to turn all the way around
(define TURN-RATE (degrees->radians (/ 360 90)))

;zero is the open space in our world
(define EMPTY 0)

;size of the scene
(define SCENE-WIDTH 300)
(define SCENE-HEIGHT 300)

#| The world (aka Grid):
  Each number maps to a color
  0 is open space |#
(define WORLD 
  '#(
     #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) 
     #(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
     #(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
     #(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
     #(1 0 0 0 0 0 2 2 2 2 2 0 0 0 0 3 0 3 0 3 0 0 0 1) 
     #(1 0 0 0 0 0 2 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 1) 
     #(1 0 0 0 0 0 2 0 0 0 2 0 0 0 0 3 0 0 0 3 0 0 0 1) 
     #(1 0 0 0 0 0 2 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 1) 
     #(1 0 0 0 0 0 2 2 0 2 2 0 0 0 0 3 0 3 0 3 0 0 0 1) 
     #(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
     #(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
     #(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
     #(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
     #(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
     #(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
     #(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
     #(1 4 4 4 4 4 4 4 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
     #(1 4 0 4 0 0 0 0 4 0 0 0 0 0 0 6 0 0 0 6 0 0 0 1) 
     #(1 4 0 0 0 0 5 0 4 0 0 0 0 0 0 0 7 0 7 0 0 0 0 1) 
     #(1 4 0 4 0 0 0 0 4 0 0 0 0 0 0 0 0 6 0 0 0 0 0 1) 
     #(1 4 0 4 4 4 4 4 4 0 0 0 0 0 0 0 7 0 7 0 0 0 0 1) 
     #(1 4 0 0 0 0 0 0 0 0 0 0 0 0 0 6 0 0 0 6 0 0 0 1) 
     #(1 4 4 4 4 4 4 4 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
     #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))

;number->color hashmap
(define colors #hash((1 . "gray")
                     (2 . "red")
                     (3 . "blue")
                     (4 . "yellow")
                     (5 . "purple")
                     (6 . "green")
                     (7 . "orange")))

;size of our level
(define WORLD-WIDTH (vector-length WORLD))
(define WORLD-HEIGHT WORLD-WIDTH)

(define VIEWING-DISTANCE (/ (/ SCENE-WIDTH 2) (tan (/ FOV 2))))

;delta between each ray cast
(define RAY-DELTA (/ FOV SCENE-WIDTH))

;draws the scene, adding one line at a time
(define (render w)
  (match-define (list pos viewing-angle) w)
  (define angle-starting-offset (viewing-angle . - . (/ FOV 2)))
  
  ;layer each line on top of the scene
  (for/fold ([s (overlay/align "left" "bottom"
                               ;gray out floor
                               (rectangle SCENE-WIDTH (/ SCENE-HEIGHT 2) 
                                          "solid" (color 128 128 128 32))
                               ;start w/ empty scene
                               (empty-scene SCENE-WIDTH SCENE-HEIGHT))])
            ([x SCENE-WIDTH])
    (define ang (angle-guard (angle-starting-offset . + . (* x RAY-DELTA))));get current angle

    ;calculate distance & type for closest horiz. & vert. wall
    (define lst (map (lambda (x) (cons (distance pos x) (wall-type x)))
                     ;find closest horizontal and vertical walls
                     (list (check-horizontal pos ang) (check-vertical pos ang))))
    ;pick the closest of the two
    (match-define (cons wall-distance the-wall-type) (argmin car lst))
    ;calculate height
    (define ht (pixel-ref-guard ((GRID-SIZE . / . wall-distance) . * . VIEWING-DISTANCE)))

    (define top (- (/ SCENE-HEIGHT 2) (/ ht 2)))
    ;add line to scene
    (scene+line s (- SCENE-WIDTH x) (+ top ht) (- SCENE-WIDTH x) top
                (hash-ref colors the-wall-type))))


(define (ray-up? ang)
  (if (and (ang . >= . 0)
           (ang . <= . (degrees->radians 180)))
      #t
      #f))

(define (ray-right? ang)
  (if (and (ang . >= . (degrees->radians 90))
           (ang . <= . (degrees->radians 270)))
      #f
      #t))

;to prevent divide by zero errors
(define (check-zero x)
  (if (= x 0)
      0.0000001
      x))

;get next horizontal wall intersection point
(define (check-horizontal pos ang #:debug [debug #f])
  (when debug (printf "Horiz: "))
  
  ;we check for collisions where our line intersects the grid
  (define (get-next-box-intersect pos)
    (let y ([y (posn-y pos)])
      (if (ray-up? ang)
              (- (* (floor (/ y GRID-SIZE))
                    GRID-SIZE)
                 1)
              (+ (* (floor (/ y GRID-SIZE))
                    GRID-SIZE)
                 GRID-SIZE))))
   
  (define y (get-next-box-intersect pos))
  ;use trig to get corresponding x coord
  (define x (+ (posn-x pos)
               (/ (- (posn-y pos) y)
                  (tan ang))))

  (define delta-y (if (ray-up? ang) (- GRID-SIZE) GRID-SIZE))
  (define delta-x (if (not (ray-up? ang))
                      (- (GRID-SIZE . / . (tan ang)))
                      (GRID-SIZE . / . (tan ang))))

  (when debug
    (printf "X is ~a, Y is ~a\n" x y)
    (printf "Delta X is ~a, Delta Y is ~a\n" delta-x delta-y))

  ;loop until we find the next wall
  (let loop ([xp x][yp y])
    (when debug (printf "---Raw-x: ~a||Raw-y ~a (Grid pos: ~a)\n" xp yp
                        (real-coord->grid-coord (posn xp yp))))
    (cond [(wall? xp yp) => identity ]
          [else (loop (+ xp delta-x) (+ yp delta-y))])))

;get next vertical wall intersection point
(define (check-vertical pos ang #:debug [debug #f])
  (when debug (printf "Vertical: "))
  
  (define (get-next-box-intersect pos ang)
    (let ([x (posn-x pos)])
          (if (ray-right? ang)
              (+ (* (floor (/ x GRID-SIZE))
                    GRID-SIZE)
                 GRID-SIZE)
              (- (* (floor (/ x GRID-SIZE))
                    GRID-SIZE)
                 1))))
 
  (define x (get-next-box-intersect pos ang))
  (define y ((posn-y pos) . + . (((posn-x pos) . - . x) . * . (tan ang))))
  
  (define delta-x (if (ray-right? ang) GRID-SIZE (- GRID-SIZE)))
  (define delta-y (if (not (ray-right? ang))
                      (GRID-SIZE . * . (tan ang))
                      (- (GRID-SIZE . * . (tan ang)))))
                          
  
  (when debug (printf "X is ~a, Y is ~a\n" x y)
    (printf "Delta X: ~a, Delta Y is ~a\n" delta-x delta-y))
  
  (let loop ([xp x][yp y])
    (when debug (printf "---Raw-x: ~a||Raw-y: ~a (Grid posn: ~a)\n" xp yp 
                        (real-coord->grid-coord (posn xp yp))))
    (cond [(wall? xp yp) => identity ]
          [else (loop (+ xp delta-x) (+ yp delta-y))])))


(define (distance p1 p2)
  (match-define (posn x1 y1) p1)
  (match-define (posn x2 y2) p2)
  (sqrt (+ (* (- x1 x2) (- x1 x2))
           (* (- y1 y2) (- y1 y2)))))

;converts float pair or single float into grid coord pair or grid point
(define (real-coord->grid-coord x)
  (match x
    [(struct posn _) (posn (quotient (inexact->exact (round (posn-x x))) GRID-SIZE)
                           (quotient (inexact->exact (round (posn-y x))) GRID-SIZE))]
    [_ (quotient x GRID-SIZE)]))

;keep grid references in bounds
(define (grid-ref-guard x)
  (define x~ (inexact->exact (round x)))
  (define MAX ((* GRID-SIZE WORLD-WIDTH) . - . 1))
  (cond [(x~ . < . 0) 0]
        [(x~ . >= . MAX) MAX]
        [else x~]))

;takes raw x y coords, returns same coords if a wall, else #f
(define (wall? x y)
  (let ((x~ (grid-ref-guard x))
        (y~ (grid-ref-guard y)))
    (if (not (= (vector-ref (vector-ref WORLD (real-coord->grid-coord y~))
                            (real-coord->grid-coord x~))
                EMPTY))
        (posn x~ y~)
        #f)))

;takes a posn
(define (wall-type pos)
  (match-define (posn x y) pos)
  (let ((x~ (grid-ref-guard x))
        (y~ (grid-ref-guard y)))
    (vector-ref (vector-ref WORLD (real-coord->grid-coord y~)) (real-coord->grid-coord x~))))

;wrap angle
(define (angle-guard x)
  (cond [(> x (* 2 pi)) (- x (* 2 pi))]
        ;we add since the angle is already negative!
        [(< x 0) (+ (* 2 pi) x)]
        [else x]))

; posn angle -> new posn
(define (walking-guard pos angle dir)
  (define op (if (equal? dir 'up)
                 +
                 -))
  (let ((new-x (op (posn-x pos) (* SPEED (cos angle))))
        ;invert y since to go up we need to subtract
        (new-y (op (posn-y pos) (- (* SPEED (sin angle))))))
    (if (wall? new-x new-y)
        pos ;don't run into a wall
        (struct-copy posn pos 
                     [x new-x]
                     [y new-y]))))

;keep all pixel references within range of our viewport
(define (pixel-ref-guard x)
  (cond [(< x 0) 0]
        [(> x (sub1 SCENE-HEIGHT)) (sub1 SCENE-HEIGHT)]
        [else x]))

;Handles keyboard input
(define (press w k)
  (match-define (list pos ang) w)
  (cond [(key=? "up" k)
         (list (walking-guard pos ang 'up) ang)]
        [(key=? "down" k) 
         (list (walking-guard pos ang 'down) ang)]
        [(key=? "left" k) (list pos (angle-guard (+ ang TURN-RATE)))]
        [(key=? "right" k) (list pos (angle-guard (- ang TURN-RATE)))]
        [else w]))


;run it!
(big-bang
 (list pos viewing-angle);state
          (on-key press);movement
          #;(state #t)
          (to-draw render)) ;rendering




;For testing, cast a ray from a pos & angle. Prints out the intermediates and results.
(define (cast pos ang)
  (printf "Grid position: ~a \n" (real-coord->grid-coord pos))
  (printf "Center angle ( in deg.): ~a\n" (radians->degrees ang))
  
  (define r-angle (ang . - . (/ FOV 2)))
  ((λ(x) (printf "View-port spread:\n First ~a\n Center ~a\n Last ~a\n"
                 (radians->degrees (first x)) 
                 (radians->degrees (list-ref x (quotient (length x) 2)))
                 (radians->degrees (last x))))
   (reverse (foldl
             (λ(x res) (cons (r-angle . + . (* x RAY-DELTA)) res))
             '()
             (range SCENE-WIDTH))))
  
  (map (lambda (x) (list x (cons (real-coord->grid-coord (posn-x x))
                                 (real-coord->grid-coord (posn-y x)))
                         (distance pos x) (wall-type x)))
       (list (check-horizontal pos ang #:debug #t)
             (check-vertical pos ang #:debug #t))))
