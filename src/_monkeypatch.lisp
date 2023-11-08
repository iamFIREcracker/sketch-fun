(in-package #:sketch)

;; https://github.com/vydd/sketch/pull/109
(defun point (x y)
  (declare (type real x y))
  (let ((weight (or (pen-weight (env-pen *env*)) 1)))
    (with-pen (make-pen :fill (pen-stroke (env-pen *env*)))
      (circle x y (/ weight 2)))))

;; https://github.com/vydd/sketch/pull/106
(defun make-line (x1 y1 x2 y2)
  (let* ((a (atan (- y2 y1) (- x2 x1)))
         (w (/ (or (pen-weight (env-pen *env*)) 1) 2))
         (dx (* (sin a) w))
         (dy (* (cos a) w)))
    (lambda ()
      (draw-shape
       :triangle-strip
       `((,(- x1 dx) ,(+ y1 dy))
         (,(- x2 dx) ,(+ y2 dy))
         (,(+ x1 dx) ,(- y1 dy))
         (,(+ x2 dx) ,(- y2 dy)))
       nil))))
