#lang racket
(require racket/gui/base)
(require racket/draw)

; === CONSTANTS ===

(define WINDOW_NAME "flappy blords")
(define FRAME_WIDTH 600)
(define FRAME_HEIGHT 960)
(define FPS 40) ;frames per second
(define MSPF (/ 1000 FPS)) ;milliseconds per frame

(define GRAV_ACCEL 1300) ;acceleration due to gravity in pixels
(define JUMP_YVEL 600) ;initial velocity after jump / spacebar

(define GAP_SIZE 200) ;gap size
(define PIPE_WIDTH 150) ;the thickness of the pipe
(define PIPE_SPEED 25) ;speed of the pipe moving to the left
(define PIPE_SPAWN_X 700) ;pipe spawn location


(define TIME_START (current-milliseconds)) ;when the program started

; === GLOBALS ===

(define game_active #f) ;determines whether the game is started or not
(define deltaTime 0) ;time between frames
(define currentTime (current-milliseconds))
(define previousTime (current-milliseconds))

; === BASIC ===

; Create Frame
(define frame (new frame% [label WINDOW_NAME] [width FRAME_WIDTH] [height FRAME_HEIGHT]))

;Rectangles
(struct rectangle ([x #:mutable] [y #:mutable] [w #:mutable] [h #:mutable]))

(define (draw-rect dc brush pen rect)
  (send dc set-brush brush)
  (send dc set-pen pen)
  (send dc draw-rectangle
        (rectangle-x rect)
        (rectangle-y rect)
        (rectangle-w rect)
        (rectangle-h rect)
        ) 
  )

;collision detection
(define (colliding rect1 rect2)
    (define x1 (rectangle-x rect1))
    (define x2 (rectangle-x rect2))
    (define y1 (rectangle-y rect1))
    (define y2 (rectangle-y rect2))
    (define w1 (rectangle-w rect1))
    (define w2 (rectangle-w rect2))
    (define h1 (rectangle-h rect1))
    (define h2 (rectangle-h rect2))

    (and (< x1 (+ x2 w2))
        (> (+ x1 w1) x2)
        (< y1 (+ y2 h2))
        (> (+ h1 y1) y2))
)
(define (bird-hit-pipe bird pip)
  (or (colliding bird (pipe-top pip)) (colliding bird (pipe-bottom pip)))
  )
;Brush Styles
(define pen-none (new pen% [style 'transparent]))
(define brush-bird (new brush% [color "red"] [style 'solid]))
(define brush-pipe (new brush% [color "green"] [style 'solid]))

; === START / LOSE ===

(define (draw-start dc)
  (send dc set-scale 3 3)
  (send dc set-text-foreground "blue")
  (send dc draw-text "Press [SPACE] to start" 0 0)
  )
(define (draw-lose dc)
  (send dc set-scale 3 3)
  (send dc set-text-foreground "red")
  (send dc draw-text "You lost. Try again with [SPACE]" 0 0)
  )

(define (start-game)
  (set! game_active #t)
  (set! time-last-jump (current-milliseconds))
 )

(define (lose-game)
  (set! game_active #f)
  )

; === BIRD ===

(define bird (rectangle (/ FRAME_WIDTH 8) (/ FRAME_HEIGHT 2) 40 40))
(define bird-prev-y (rectangle-y bird))

(define (draw-bird dc)
  (draw-rect dc brush-bird pen-none bird))

;=== pipe ===
(struct pipe([top #:mutable] [bottom #:mutable]))

(define (create-pipe)
  (define top-height (* (random) (- (- FRAME_HEIGHT GAP_SIZE) 150)))
  (define top (rectangle PIPE_SPAWN_X 0 PIPE_WIDTH top-height))
  (define bottom (rectangle PIPE_SPAWN_X (+ top-height GAP_SIZE) PIPE_WIDTH ( - FRAME_HEIGHT (+ top-height GAP_SIZE))))
  (pipe top bottom)
  )

(define (set-pipe-x x pip)
  (set-rectangle-x! (pipe-top pip) x)
  (set-rectangle-x! (pipe-bottom pip) x)
  
  )

(define (draw-pipe dc pip)
  (draw-rect dc brush-pipe pen-none (pipe-top pip))
  (draw-rect dc brush-pipe pen-none (pipe-bottom pip))
  )

(define testpipe (create-pipe))
 
; === PHYSICS / MOVEMENT ===

;Time when the bird last jumped
(define time-last-jump TIME_START)

;Time since the bird last jumped
(define (since-last-jump)
  (- time-last-jump (current-milliseconds)))

; Make bird fall

(define (get-delta-y)
  (define time (/ (since-last-jump) 1000))
  (+ (* JUMP_YVEL time) (/ (* GRAV_ACCEL (* time time)) 2))
 )

(define (get-current-y)
  (+ (get-delta-y) bird-prev-y)
 )

(define (setpos-bird)
  ;set bird position
  (set-rectangle-y! bird (get-current-y))

  ;if bird hits floor, lose game
  (if (> (rectangle-y bird) FRAME_HEIGHT)
      (lose-game)
      "Do nothing"
  )

  ;if bird hits pipe, lose game.
  (if (bird-hit-pipe bird testpipe)
      (lose-game)
      "Do nothing"
      )
  )

; Make bird jump

(define (jump)
  (set! bird-prev-y (get-current-y))
  (set! time-last-jump (current-milliseconds))
  )

; make pipe move

(define (move_pipe pip)
(define current_x (rectangle-x (pipe-top pip)))
  (set-pipe-x (- current_x (* PIPE_SPEED deltaTime)) pip)
  )

; === MAIN DRAW ===
; drawn every frame
(define (draw-game dc)
  (send dc set-scale 1 1)
  (if game_active
      (begin
        (draw-bird dc)
        (draw-pipe dc testpipe)
       )
      (begin
        (draw-start dc)
        )
      )
  )

; === GUI ===

; Keyboard Event
(define (keyboard-event event)
  (if (equal? (send event get-key-code) #\space)
      (if game_active
          (jump)
          (start-game)
          )
      "Do Nothing"
      )
  )

;Canvas with custom keyboard event
(define sb-canvas%
  (class canvas%
    (define/override(on-char event)
      (keyboard-event event))
    (super-new)))


;Initialize canvas with custom paint callback
(define canvas
  (new sb-canvas%
       [parent frame]
       [paint-callback (λ (canvas dc) (draw-game dc))]))

;Display Window
(send frame show #t)

; === GAME LOOP ===

(define (gameloop)
  (set! currentTime (current-milliseconds))
  (set! deltaTime (/ (- currentTime previousTime) 100))
  (if game_active
      (begin  
        ;Physics Update
        (setpos-bird)
        (move_pipe testpipe)
        ;Redraw all objects
        (send frame refresh)
      )
      (send frame refresh)
  )
  (set! previousTime currentTime)
  )

(define timer (new timer% [notify-callback gameloop] [interval MSPF]))

