(uiop:define-package #:doomfire
  (:use #:cl #:sketch)
  (:export
   start
   clear
   stop))
(in-package #:doomfire)


(defparameter *width* 600)
(defparameter *height* 400)
(defparameter *scale* 8)
(defparameter *colors*
  (list "#070707" "#1F0707" "#2F0F07" "#470F07" "#571707" "#671F07" "#771F07"
        "#8F2707" "#9F2F07" "#AF3F07" "#BF4707" "#C74707" "#DF4F07" "#DF5707"
        "#DF5707" "#D75F07" "#D75F07" "#D7670F" "#CF6F0F" "#CF770F" "#CF7F0F" 
        "#CF8717" "#C78717" "#C78F17" "#C7971F" "#BF9F1F" "#BF9F1F" "#BFA727" 
        "#BFA727" "#BFAF2F" "#B7AF2F" "#B7B72F" "#B7B737" "#CFCF6F" "#DFDF9F" 
        "#EFEFC7" "#FFFFFF"))
(defparameter *palette*
  (coerce (loop :for hex :in *colors*
                :collect (make-pen :fill (hex-to-color hex)))
          'array))

(defsketch doomfire ((title "Doom's Fire")
                 (width *width*)
                 (height *height*)
                 (framebuffer (default-frameuffer *width* *height* *scale*)))
  (draw-fire sketch::instance)
  (tick-fire sketch::instance))

(defun default-frameuffer (width height scale)
  (let* ((rows (floor height scale))
         (cols (floor width scale))
         (framebuffer (make-array (list rows cols)))
         (black 0)
         (white (1- (length *palette*))))
    (loop :for r :below rows :do
          (loop :for c :below cols :for last-row-p = (< r (1- rows)) :do
                (setf (aref framebuffer r c) (if last-row-p black white))))
    framebuffer))

(defun draw-fire (window)
  (with-slots (framebuffer) window
    (loop :for r :below (array-dimension framebuffer 0) :do
          (loop :for c :below (array-dimension framebuffer 1) :do
                (let ((color (aref framebuffer r c)))
                  (with-pen (aref *palette* color)
                    (rect (* c *scale*) (* r *scale*) *scale* *scale*)))))))

(defun tick-fire (window)
  (with-slots (framebuffer) window
    (let ((rows (array-dimension framebuffer 0))
          (cols (array-dimension framebuffer 1)))
      (loop :for r :from 1 :below rows :do
            (loop :for c :below cols
                  :for from = (+ (* r cols) c) :do
                  (spread-fire framebuffer from))))))

(defun spread-fire (framebuffer from)
  (let* ((rows (array-dimension framebuffer 0))
         (cols (array-dimension framebuffer 1))
         (rand (random 3))
         (to (- from cols (1- rand)))
         (color (max (- (row-major-aref framebuffer from) (logand rand 3 #+#:excluded 1)) 0)))
    (when (and (<= 0 to) (< to (* cols (1- rows))))
      (setf (row-major-aref framebuffer to) color))))


(defvar *window* nil)

(defun start ()
  (unless *window*
    (setf *window* (make-instance 'doomfire))))

(defun clear()
  (with-slots (framebuffer) *window*
    (setf framebuffer (default-frameuffer *width* *height* *scale*)))
  (values))

(defun stop ()
  (when *window*
    (kit.sdl2:close-window *window*)
    (prog1 *window*
      (setf *window* nil))))
