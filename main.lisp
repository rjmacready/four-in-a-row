
;(defpackage :4row
;  (:use :cl)
;  (:exports :check-wins
;	    :*board-plays*))

;(in-package :4row)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload 'lispbuilder-sdl)
  (ql:quickload 'lispbuilder-sdl-ttf)

  (defun flatten (ls)
    "Flatten a list of lists: ((a) (b)) becomes (a b)"
    (reduce #'append ls))

  ; Size of the board
  ; no need to clean
  (defparameter *width* 7)
  (defparameter *height* 8)

  (defparameter *board-plays* NIL)
  (defparameter *board-cells*
    (flatten (loop for x from 1 to *width* collect (loop for y from 1 to *height* collect `(,x ,y)))))

  (defun index-cell-board (board-cells cell)
    (position cell board-cells :test #'equal))

  (defun index-cell (cell)
    (index-cell-board *board-cells* cell))
  
  (defun get-column-board (board column)
    (remove-if-not (lambda (cell) (= (car cell) column)) board))

  (defun get-column (column)
    (get-column-board *board-cells* column))

  (defun check-if-cell-not-played (cell)
    (= 0 (elt *board-plays* (index-cell cell))))
  )


; Events to flush
(defparameter *events* NIL)
; Position of the pointer [1 .. width]
(defparameter *x* NIL)
; Current player. 1 or 2
(defparameter *player* NIL)
; :playing, :won-by-1, :won-by-2
(defparameter *status* NIL)

(defun switch-player ()
  (if (= *player* 1)
      (setf *player* 2)
      (setf *player* 1)))

;(defconstant W 7)
;(defconstant H 8)

(defun inc-x (delta)
  "Updates player's x, [1 .. width]"
  (let ((new-x (+ *x* delta))) 
    (when (> new-x *width*)
      (setf new-x 1))
    (when (< new-x 1)
      (setf new-x *width*))
    (setf *x* new-x)))


(defun set-nth (ls nth new-elem)
  "Set nth element of a list. Returns a new list!"
  (if (= nth 0)
      (cons new-elem (cdr ls))
      (cons (car ls) (set-nth (cdr ls) (- nth 1) new-elem))))

(defun check-column-can-play (column)
  "Returns (true, freecells) if we can play in a given column in a given board; 
otherwise returns (NIL, NIL)"
  (let* ((col (get-column-board *board-cells* column))
	 ; filter cells in column which are not played
	 (free-cells (remove-if-not #'check-if-cell-not-played col)))
    (values (not (null free-cells)) free-cells)))


(defun play-at-column-board (board player column)
  "Plays at a given column in a given board. If it is not possible to play in a column,
returns NIL; otherwise, plays and returns the new board"    
  (multiple-value-bind (can-play free-cells) (check-column-can-play column)
    (if can-play
	; lets take the last free cell, and play in it!
	(let* ((the-last (car (last free-cells)))
	       (idx (index-cell the-last))
	       (new-board (set-nth board idx player)))
	  new-board)
	NIL)
    ))

(defun play-at-column (player column)
  "Plays at a given column of the *board-plays*.
If the play is valid, returns T and sets the new *board-plays*; oherwise returns NIL."
  (let ((after-play (play-at-column-board *board-plays* player column)))
    (if (null after-play)
	nil
	(progn
	  (setf *board-plays* after-play)
	  t))))

;(profile play-at-column)

;; Generate acessors for each valid row, column and diagonal.
;; Then transverse these acessors.

; horizontal acessors

; there are 56 cells
; a column has <height> cells

(defun check-board-4row (indexes)
  ; get head. if is zero we can exit immediatly
  (let ((head (elt *board-plays* (car indexes))))
    (if (= head 0)
	nil
	(if (loop for idx in (cdr indexes)
	     for play = (elt *board-plays* idx)
	     always (= play head)
	     )
	    (multiple-value-bind (r) (intern (write-to-string head) "KEYWORD")
				  r)	    
	    nil)
	)))

(defmacro make-check-wins ()
  (let ((vertical 
	 (flatten 
	  (loop for x from 1 to *width* collect  ;*width*
	       (loop for y from 1 to (+ 1 (- *height* 4)) collect ; *height*
		    (loop for c from 0 to 3
		       collect `(,x ,(+ y c)))))))
	
	(horizontal
	 (flatten
	  (loop for y from 1 to *height* collect ;*height*
	       (loop for x from 1 to (+ 1 (- *width* 4)) collect ; *width*
		    (loop for c from 0 to 3
		       collect `(,(+ x c) ,y))))))
    
	(diag-1
	 (flatten 
	  (loop for x from 1 to (+ 1 (- *width* 4)) collect  ; *width*
	       (loop for y from 1 to (+ 1 (- *height* 4)) collect  ; *height*
		    (loop for c from 0 to 3 collect `(,(+ x c) ,(+ y c)))))))
    
	(diag-2
	 (flatten
	  (loop for x from (+ 1 (- *width* 4)) to *width* collect ; *width*
	       (loop for y from 1 to (+ 1 (- *height* 4)) collect ; *height*
		    (loop for c from 0 to 3 collect `(,(- x c) ,(+ y c))))))))
    
    (let* ((all (append vertical horizontal diag-1 diag-2))
	   (all (mapcar (lambda (_) (mapcar #'index-cell _)) all)))
      `(defun check-wins (board)
		 
		 (let ((res
			(loop for chance in ',all
			   for res = (check-board-4row chance)
			   when (not (null res)) return res
			     )))
		   (if res
		       res
		       (if (null (remove-if-not (lambda(_) (= _ 0)) board)) ; check if is a draw!
			   :draw
			   :playing) 
		       ))		     
		 )
      )
    ))

(make-check-wins)

; "index" of board cells centers
; no need to clean
(defparameter *circle-centers*
  (mapcar (lambda (x)
	    (let ((a (first x)) (b (second x)))
	      `(,(+ (* a 30) 0) ,(+ (* b 30) 40))))
	  *board-cells*))

(defun do-event (event)
  "Processes stuff for an event"
  (cond 
    ((eq event :move-left) (inc-x -1))
    ((eq event :move-right) (inc-x 1))
    ((eq event :play) 
     (progn
       (when (play-at-column *player* *x*)
	 ;(print `(:state := ,(check-wins *board-plays*)))
	 (let ((status (check-wins *board-plays*)))
	   (cond 
	     ((eq status :draw) (setf *status* :draw))
	     ((eq status :1) (setf *status* :won-by-1))
	     ((eq status :2) (setf *status* :won-by-2))
	     ((eq status :playing)
	      (switch-player))
	     )
	   ))
       ))
    ))

(defun flush-events ()
  "Pops and processes events"
  (if (null *events*)
      t
      (let ((event (pop *events*)))
	(do-event event)
	(flush-events))))


(defun draw-circle (cell)
  "Draws a cell"
  (let* ((idx (index-cell cell))
	 (play (elt *board-plays* idx))
	 (pt (elt *circle-centers* idx))
	 (x (first pt)) 
	 (y (second pt)))
    (cond
      ((= 0 play)
       (sdl:draw-circle (sdl:point :x x :y y) 10
		     :surface sdl:*default-display*
		     :color (sdl:color :r 255 :g 255 :b 255)))

      ((= 1 play)
       (sdl:draw-filled-circle
	  (sdl:point :x x :y y) 10
		     :surface sdl:*default-display*
		     :color (sdl:color :r 255 :g 255 :b 0)
		     :stroke-color (sdl:color :r 255 :g 255 :b 255)
	 ))

      ((= 2 play)
       (sdl:draw-filled-circle
	(sdl:point :x x :y y) 10
		     :surface sdl:*default-display*
		     :color (sdl:color :r 255 :g 0 :b 255)
		     :stroke-color (sdl:color :r 255 :g 255 :b 255)
	 ))
    
    )))


(defparameter *font-normal* NIL)

(defun game-loop()
  "Stuff per-game loop; flush events, draws stuff"

  ;;; Pop events; update game state!
  (flush-events)
  
  ;;; Draw stuff. At this point, the game state must be updated!
  (sdl:clear-display (sdl:color :r 0 :g 0 :b 0) :surface sdl:*default-display*)

  (sdl:draw-string-solid-* "Play!" 20 20 :font *font-normal* :color sdl:*white*)

  ; draw triangle (user)
  (let ((t-x (* 30 *x*))) 
    (sdl:draw-trigon 
     (sdl:point :x (- t-x 5) :y 40) 
     (sdl:point :x (+ t-x 5) :y 40) 
     (sdl:point :x t-x :y 50)
     :surface sdl:*default-display*
     :color (sdl:color :r 0 :g 255 :b 255)))

  ; draw cells
  (mapc #'draw-circle *board-cells*)

  (cond
    ((eq *status* :won-by-1) 
     ;(print '(:won :by :1))
     (sdl:draw-string-solid-* "Player 1 won!" 100 20
			      :font *font-normal*
			      :color sdl:*red*)
     )
    ((eq *status* :won-by-2)
     ;(print '(:won :by :2))
     (sdl:draw-string-solid-* "Player 2 won!" 100 20
			      :font *font-normal*
			      :color sdl:*red*)
     )
    ((eq *status* :draw)
     (sdl:draw-string-solid-* "Draw!!!" 100 20
			      :font *font-normal*
			      :color sdl:*red*))
    )
  
  ; updates display
  (sdl:update-display)
  t)

(defun clean-env ()
  (setf *board-plays* (loop for x in *board-cells* collect 0))
  (setf *x* 4)
  (if (or (null *player*) (= *player* 2))
      (setf *player* 1)
      (setf *player* 2))
  (setf *events* NIL)
  (setf *status* :playing))

; main-loop
(sdl:with-init ()  
  (sdl:window 720 480)
  (sdl:enable-key-repeat 500 100)
  (setf *font-normal* (sdl:initialise-font sdl:*font-8x8*))
  ; start a game
  (clean-env)
  (game-loop)
  (sdl:with-events ()
    (:quit-event () t)
    (:key-down-event (:key key)
		     (when (sdl:key= key :sdl-key-escape)
		       (sdl:push-quit-event))
		     (cond
		       ((eq *status* :playing)

			(when (sdl:key= key :sdl-key-left)
			  (push :move-left *events*))
			(when (sdl:key= key :sdl-key-right)
			  (push :move-right *events*))
			(when (sdl:key= key :sdl-key-space)
			  (push :play *events*)))
		       (t (clean-env))
		       )
		     )
    (:idle () 
	   (game-loop))
    ))