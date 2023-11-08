(uiop:define-package #:10print
  (:use #:cl #:sketch #:3am #:mlutils)
  (:export
   start
   clear
   stop))
(in-package #:10print)


;; Screen buffer -- a ring buffer ---------------------------------------------

(defclass screen-buffer ()
  ((rows :initarg :rows
         :initform (error ":ROWS is required"))
   (cols :initarg :cols
         :initform (error ":COLS is required"))
   (elements)
   (first-idx :initform 0)
   (last-idx :initform 0)))

(defmethod initialize-instance :after ((instance screen-buffer) &key rows cols)
  (with-slots (size elements) instance
    (setf elements (make-array (1+ (* rows cols))
                               :element-type 'integer))))

(defmethod full? ((instance screen-buffer))
  (with-slots (elements first-idx last-idx) instance
    (= (- last-idx first-idx) (1- (length elements)))))

(defmethod push-char ((instance screen-buffer) elem)
  "Adds `elem` to ELEMENTS.

  If the screen buffer is full, the oldest COLS elements are discarded. "
  (with-slots (cols elements first-idx last-idx) instance
    (when (full? instance)
      (incf first-idx cols))
    (setf (aref elements (mod last-idx (length elements))) elem)
    (incf last-idx)))

(examples screen-buffer
  (bnd1 (instance (make-instance 'screen-buffer :rows 2 :cols 2))
    ;; Empty buffer is not full
    (is (not (full? instance)))
    ;; Not full until you push ROWS times COLS characters
    (push-char instance 205)
    (is (not (full? instance)))
    (push-char instance 205)
    (is (not (full? instance)))
    (push-char instance 205)
    (is (not (full? instance)))
    (push-char instance 205)
    (is (full? instance))
    ;; Not ful until you push COLS elements
    (push-char instance 205)
    (is (not (full? instance)))
    (push-char instance 205)
    (is (full? instance))))

(defmacro doscreenbuffer (((var-elem var-cur-row var-cur-col) screen-buffer) &body body)
  (bnd* ((gs-idx (gensym "idx"))
         (gs-rows (gensym "rows"))
         (gs-cols (gensym "cols"))
         (gs-elements (gensym "elements"))
         (gs-first-idx (gensym "first-idx"))
         (gs-last-idx (gensym "last-idx")))
    `(with-slots ((,gs-first-idx first-idx)
                  (,gs-last-idx last-idx)
                  (,gs-rows rows)
                  (,gs-cols cols)
                  (,gs-elements elements))
       ,screen-buffer
       (loop for ,gs-idx from ,gs-first-idx below ,gs-last-idx
             for ,var-cur-row = (floor (- ,gs-idx ,gs-first-idx) ,gs-cols)
             for ,var-cur-col = (mod (- ,gs-idx ,gs-first-idx) ,gs-cols)
             for ,var-elem = (aref ,gs-elements (mod ,gs-idx (length ,gs-elements))) do
             ,@body))))

#+#:excluded (bnd1 (instance (make-instance 'screen-buffer :rows 2 :cols 2))
               (push-char instance 205)
               (push-char instance 206)
               (push-char instance 207)
               (push-char instance 208)
               (doscreenbuffer ((ch r c) instance)
                 (print (list ch r c))))


;; The actual animation -------------------------------------------------------

(defsketch 10print ((title "10 PRINT CHR$(205.5+RND(1)); : GOTO 10")
                    (width 600)
                    (height 400)
                    (scale 1/4)
                    (rows (/ height 100 scale))
                    (cols (/ width 100 scale))
                    (screen-buffer (make-instance 'screen-buffer
                                                  :rows rows
                                                  :cols cols))
                    (bg (rgb-255 61 50 156))
                    (fg-pen (make-pen :stroke #1=(rgb-255 122 113 212) :fill #1#)))
  (background bg)
  (%print (chr$ (+ 205.5 (rnd 1)))))

(defun rnd (n) (random (* n 1.0)))

(defun chr$ (val)
  (setf val (floor val)))

(defun %print (ch)
  (with-slots (scale cols screen-buffer fg-pen) sketch::*sketch*
    (push-char screen-buffer ch)
    (draw-screen-buffer)))

(defun draw-screen-buffer ()
  (with-slots (scale cols screen-buffer fg-pen) sketch::*sketch*
    (flet ((move-top-left() (translate 0 0))
           (move-right () (translate 100 0))
           (move-start-next-line () (translate (- (* cols 100)) 100)))
      (with-pen fg-pen
        (with-scale (scale)
          (doscreenbuffer ((ch row col) screen-buffer)
            (when (= row col 0)
              (move-top-left))
            (draw-char ch)
            (move-right)
            (when (= (1+ col) cols)
              (move-start-next-line))))))))

(defun draw-char (val &aux (size 100) (weight 25))
  (ecase val
    (205 (polygon 0 0
                  weight 0
                  size (- size weight)
                  size size
                  (- size weight) size
                  0 weight))
    (206 (polygon 0.0 size
                  0 (- size weight)
                  (- size weight) 0
                  size 0
                  size weight
                  weight size))))


(defvar *window* nil)

(defun start ()
  (unless *window*
    (setf *window* (make-instance '10print))))

(defun clear()
  (values))

(defun stop ()
  (when *window*
    (kit.sdl2:close-window *window*)
    (prog1 *window*
      (setf *window* nil))))
