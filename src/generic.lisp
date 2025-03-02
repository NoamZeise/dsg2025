(in-package :dsg2025)

(defgeneric draw (game))
(defgeneric update (game dt))
(defgeneric resize (game w h))
(defgeneric cleanup (game))
(defgeneric reload (game))

(defconstant +world-res-w+ 200)
(defconstant +world-res-h+ 200)

(defconstant +ui-res-w+ 400)
(defconstant +ui-res-h+ 225)

(defconstant +base-player-height+ 5.0)

(defparameter *tiles-to-travel* 28.2)

(defparameter *time-to-end* 4)

(defparameter
 *map-layout*
 '((:x :b :b :b :b :b :b :b :x)
   (:x :b :o :r :o :o :r :b :b)
   (:x :b :b :o :o :o :o :o :b)
   (:b :b :d :b :c :b :o :r :b)
   (:b :o :o :o :b :o :o :o :b)
   (:b :o :o :o :b :o :d :b :b)
   (:b :r :b :r :o :o :r :r :b)
   (:b :o :o :o :b :o :o :b :b)
   (:b :b :b :o :o :b :o :d :b)
   (:x :b :b :o :d :o :r :o :b)
   (:x :b :o :b :o :o :o :b :b)
   (:b :b :o :o :r :o :o :o :b)
   (:b :d :r :o :b :o :r :b :b)
   (:b :b :o :o :o :o :b :b :x)
   (:x :b :b :b :o :r :b :b :x)
   (:b :b :b :o :d :o :o :b :x)
   (:b :s :o :b :o :o :b :b :x)
   (:b :o :o :r :r :b :b :x :x)
   (:x :b :b :b :b :b :x :x :x)))

(defparameter *world-map* nil)

(defun midpoint (rect)
  (destructuring-bind
   (x y w h) (gficl:vec-data rect)
   (gficl:make-vec (list (/ w 2)
			 (/ h 2)))))
