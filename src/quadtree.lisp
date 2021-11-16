(uiop:define-package #:quadtree
  (:use #:cl #:sketch)
  (:export
   start
   clear
   stop))
(in-package #:quadtree)


(defstruct (loc (:type list)
                (:constructor make-loc%))
  x y)

(defun make-loc (x y) (make-loc% :x x :y y))


(defstruct (bbox (:type list)
                 (:constructor make-bbox%))
  loc width height)

(defun make-bbox (x y width height)
  (make-bbox% :loc (make-loc x y) :width width :height height))

(defun bbox-contains-p (bbox loc)
  (destructuring-bind ((bx by) bw bh) bbox
    (destructuring-bind (lx ly) loc
      (and (<= bx lx) (< lx (+ bx bw))
           (<= by ly) (< ly (+ by bh))))))

(defun bbox-overlaps-p (bbox other)
  (destructuring-bind ((bx by) bw bh) bbox
    (destructuring-bind ((ox oy) ow oh) other
      (not (or (< (+ bx bw) ox)
               (< (+ ox ow) bx)
               (< (+ by bh) oy)
               (< (+ oy oh) by))))))


(defstruct (element (:type list))
  loc)


(defstruct (qtree (:type list)
                  (:constructor make-qtree%))
  bbox
  capacity
  elements
  sub-trees)

(defun make-qtree (bbox &optional (capacity 4))
  (make-qtree% :bbox bbox :capacity capacity))

(defun qtree-insert (qtree e)
  (when (bbox-contains-p (qtree-bbox qtree) (element-loc e))
    (if (< (length (qtree-elements qtree)) (qtree-capacity qtree))
        (push e (qtree-elements qtree))
        (progn
          (unless (qtree-subtrees qtree)
            (qtree-subdivide qtree))
          (dolist (tree (qtree-subtrees qtree))
            (qtree-insert tree e))
          t))))

(defun qtree-subdivide (qtree)
  (when (qtree-subtrees qtree)
    (error "QTREE sub-divided already!"))
  (setf (qtree-subtrees qtree)
        (destructuring-bind ((x y) w h) (qtree-bbox qtree)
          (let ((w-new (floor w 2))
                (h-new (floor h 2))
                (capacity (qtree-capacity qtree)))
            (list (make-qtree (make-bbox x           y           w-new h-new) capacity)
                  (make-qtree (make-bbox (+ x w-new) y           w-new h-new) capacity)
                  (make-qtree (make-bbox x           (+ y h-new) w-new h-new) capacity)
                  (make-qtree (make-bbox (+ x w-new) (+ y h-new) w-new h-new) capacity))))))

(defun qtree-query (qtree bbox)
  "Returns all the elements from `qtree` which are contained inside `bbox`
  bounding box.

  As a second value this function also returns the number of elements that
  had to be check to see if they were included or not in `bbox`."
  (if (not (bbox-overlaps-p (qtree-bbox qtree) bbox))
    (values nil 0)
    (let ((total-checks (length (qtree-elements qtree))))
      (values
        (append
          (remove-if-not #'(lambda (e)
                            (bbox-contains-p bbox (element-loc e)))
                         (qtree-elements qtree))
          (mapcan #'(lambda (sub-tree)
                     (multiple-value-bind (found num-checks)
                         (qtree-query sub-tree bbox)
                       (incf total-checks num-checks)
                       found))
                  (qtree-sub-trees qtree)))
        total-checks))))


(defparameter *width* 400)
(defparameter *height* 400)
(defparameter *background* (hex-to-color "#444"))

(defparameter *pen-grid* (make-pen :stroke (hex-to-color "#aaa") :weight 1))
(defparameter *pen-bbox* (make-pen :stroke (hex-to-color "#d1009c") :weight 2))
(defparameter *pen-point* (make-pen :stroke (hex-to-color "#ececec") :weight 4))
(defparameter *pen-point-in-bbox* (make-pen :stroke (hex-to-color "#d1009c") :weight 4))
(defparameter *font-info* (make-font :color (hex-to-color "#ffa724" #+#:excluded "#8cff7c") :size 20))

(defsketch quadtree ((title "Quadtree")
                     (width *width*)
                     (height *height*)
                     (search-box (default-search-box))
                     (qtree (make-qtree (make-bbox 0 0 *width* *height*)))
                     (elements-count 0)
                     (drag-n-drop-p nil))
  (background *background*)
  (draw-quadtree qtree)
  (multiple-value-bind (found checks-count)
      (qtree-query qtree search-box)
    (draw-focus-box search-box found)
    (draw-info width height elements-count checks-count)))

(defun default-search-box ()
  (make-bbox 125 150 150 100))

(defun draw-quadtree (qtree)
  (destructuring-bind ((x y) w h) (qtree-bbox qtree)
    (with-pen *pen-grid*
      (rect x y w h))
    (dolist (sub-tree (qtree-sub-trees qtree))
      (draw-quadtree sub-tree)))
  (dolist (e (qtree-elements qtree))
    (destructuring-bind (x y) (element-loc e)
      (with-pen *pen-point*
        (point x y)))))

(defun draw-focus-box (search-box found)
  (destructuring-bind ((x y) w h) search-box
    (with-pen *pen-bbox*
      (rect x y w h)))
  (dolist (e found)
    (destructuring-bind (x y) (element-loc e)
      (with-pen *pen-point-in-bbox*
        (point x y)))))

(defun draw-info (width height elements-count checks-count)
  (let ((text-string (format nil "# elements: ~d / # checks: ~d" elements-count checks-count)))
    (with-pen (make-pen :fill *background*)
      (rect 0 (- height 30) width 30))
    (with-font *font-info*
      (text text-string 10 (- height 30)))))


(defun point (x y)
  "Sketch's POINT function always draws 1x1 rectangles; here we use the
  current PEN's weight to draw bigger rectangles instead.

  https://github.com/vydd/sketch/issues/46"
  (declare (type real x y))
  (let ((weight (or (pen-weight (sketch::env-pen sketch::*env*)) 1)))
    (with-pen (make-pen :fill (pen-stroke (sketch::env-pen sketch::*env*)))
      (rect (- x (floor weight 2))
            (- y (floor weight 2))
            weight
            weight))))


(defmethod kit.sdl2:mousebutton-event ((window quadtree) state timestamp button x y)
  (declare (ignore timestamp button))
  (setf (slot-value window 'drag-n-drop-p) (eq state :mousebuttondown))
  (unless (eq state :mousebuttonup)
    (add-random-points window x y)))

(defmethod kit.sdl2:mousemotion-event ((window quadtree) timestamp button-mask x y xrel yrel)
  (declare (ignore timestamp button-mask xrel yrel))
  (when (slot-value window 'drag-n-drop-p)
    (add-random-points window x y))
  (center-bbox window x y))

(defun add-random-points (window x y)
  (with-slots (qtree elements-count) window
    (loop :repeat 5
          :for loc = (make-loc (+ x (- (random 30) 15))
                               (+ y (- (random 30) 15)))
          :do (qtree-insert qtree (make-element :loc loc))
          (incf elements-count))))

(defun center-bbox (window x y)
  (with-slots (search-box) window
    (destructuring-bind (loc w h) search-box
      (declare (ignore loc))
      (setf search-box (make-bbox (- x (floor w 2))
                                  (- y (floor h 2))
                                  w
                                  h)))))


(defvar *window* nil)

(defun start ()
  (unless *window*
    (setf *window* (make-instance 'quadtree))))

(defun clear()
  (with-slots (search-box qtree elements-count) *window*
    (setf search-box (default-search-box)
          qtree (make-qtree (make-bbox 0 0 *width* *height*) 4)
          elements-count 0))
  (values))

(defun stop ()
  (when *window*
    (kit.sdl2:close-window *window*)
    (prog1 *window*
      (setf *window* nil))))
