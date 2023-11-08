(uiop:define-package #:tanlines
  (:use #:cl #:sketch #:3am #:mlutils)
  (:export
   start
   clear
   stop))
(in-package #:tanlines)

(load (merge-pathnames "./_monkeypatch.lisp" *load-truename*))

(defparameter *samples* 1700)

(defun points ()
  (uiop:while-collecting (acc)
    (dolist (x (range -1 1 :step (/ 2 *samples*)))
      (acc (cons x (* (cos (* x 14)) 0.09))))))


(defun tanlines (points &optional (ratio 1))
  (uiop:while-collecting (acc)
    (dolists (((x1 . y1) points)
              ((x2 . y2) (cdr points)))
      (bnd* ((slope (/ (- y1 y2) (- x1 x2)))
             (intercept (- y1 (* slope x1))))
        (flet ((y (x)
                 (+ (* slope x) intercept)))
          (acc (list (- ratio) (y (- ratio))
                     ratio (y ratio))))))))


(defsketch tanlines ((title "Tangent lines")
                     (width 1200)
                     (height 800)
                     (y-axis :up)
                     (ratio (/ width height))
                     )
  (background +white+)
  (scale (/ width 2 ratio) (/ height 2))
  (translate ratio 1)
  (with-pen (make-pen :weight (/ 1 height))
    (bnd* ((points (points))
           (tanlines (tanlines points ratio)))
      (with-pen (make-pen :stroke (rgb 0 0 0 0.4) :weight (/ 1 height))
        (dolist+ ((x1 y1 x2 y2) tanlines)
          (line x1 y1 x2 y2)))
      (with-pen (make-pen :stroke (rgb 1 0 0 0.9) :weight (/ 6 height))
        (dolists (((x1 . y1) points)
                  ((x2 . y2) (cdr points)))
          (line x1 y1 x2 y2))))))


(defvar *window* nil)

(defun start ()
  (unless *window*
    (setf *window* (make-instance 'tanlines))))

(defun clear()
  (values))

(defun stop ()
  (when *window*
    (kit.sdl2:close-window *window*)
    (prog1 *window*
      (setf *window* nil))))
