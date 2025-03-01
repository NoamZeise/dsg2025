(in-package :dsg2025)

(defgeneric draw (game))
(defgeneric update (game dt))
(defgeneric resize (game w h))
(defgeneric cleanup (game))
(defgeneric reload (game))

(defconstant +world-res-w+ 400)
(defconstant +world-res-h+ 400)
