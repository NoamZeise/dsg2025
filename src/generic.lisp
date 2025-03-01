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


(defparameter *world-map*
  '((:camp     :boulders :boulders :plain     :boulders)
    (:plain    :plain    :depot    :plain     :boulders)
    (:boulders :boulders :start    :plain     :boulders)
    (:boulders :plain    :depot    :boulders  :boulders)))
