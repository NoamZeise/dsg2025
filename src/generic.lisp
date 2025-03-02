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

(defparameter *tiles-to-travel* 17)

(defparameter
 *map-layout*
 '((:b :b :b :b :b :b :b :b :b)
   (:b :p :p :p :p :p :p :p :b)
   (:b :p :p :p :p :p :p :p :b)
   (:b :p :p :s :p :p :p :p :b)
   (:b :p :p :r :d :p :p :p :b)
   (:b :p :p :p :p :p :p :p :b)
   (:b :p :p :p :p :p :p :p :b)
   (:b :p :p :p :c :p :p :p :b)
   (:b :b :b :b :b :b :b :b :b)))

(defparameter
 *world-map*
 '(
   (:boulders :boulders :boulders :boulders :boulders :boulders)
   (:boulders :camp     :boulders :boulders :boulders     :boulders)
   (:boulders :plain    :plain    :depot    :plain     :boulders)
   (:boulders :boulders :start    :pressure  :boulders :boulders)
   (:boulders :plain    :plain    :plain    :plain :boulders)
   (:boulders :boulders    :boulders    :boulders  :boulders)))

(defun midpoint (rect)
  (destructuring-bind
   (x y w h) (gficl:vec-data rect)
   (gficl:make-vec (list (/ w 2)
			 (/ h 2)))))
