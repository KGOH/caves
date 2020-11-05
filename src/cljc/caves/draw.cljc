(ns caves.draw
  (:require [quil.core :as quil]
            [caves.math :as math]))


(defn draw-curve! [points opts]
  (quil/no-fill)
  (quil/stroke-weight (:weight opts))
  (apply quil/stroke (:color opts))

  (quil/begin-shape)
  (doseq [p (take (+ 3 (count points)) (cycle points))]
    (apply quil/curve-vertex p))
  (quil/end-shape))


(defn draw-polygon! [points opts]
  (quil/no-fill)
  (quil/stroke-weight (:weight opts))
  (apply quil/stroke (:color opts))

  (quil/begin-shape)
  (doseq [p points]
    (apply quil/vertex p))
  (quil/end-shape :close))


(defn draw-point! [[x y] opts]
  (quil/no-fill)
  (quil/stroke-weight 10)
  (quil/ellipse x y (:weight opts) (:weight opts)))


(defn draw-distance! [[x y] opts]
  (quil/no-fill)
  (quil/stroke-weight 1)
  (quil/ellipse x y 70 70))


(defn draw-coordinate! [[x y] opts]
  (apply quil/fill [180])
  (quil/text-font (quil/create-font "Iosevka Regular" 10))
  (quil/text (str (int x) "," (int y)) x y)
  (let [{:keys [angle radius]} (math/cartesian->polar [x y])]
    (quil/text (str (int (* quil/RAD-TO-DEG angle 10)) "," (int radius)) x (+ y 12))))


(defn draw-index! [i [x y] opts]
  (apply quil/fill [180])
  (quil/text-font (quil/create-font "Iosevka Regular" 10))
  (quil/text (str i) x (- y 10)))


(defn draw-slice! [points opts]
  (if (get-in opts [:settings :debug :lines])
    (draw-polygon! points opts)
    (draw-curve! points opts))

  (doseq [[index point] (map-indexed vector points)]
    (when (get-in opts [:settings :debug :points])
      (draw-point! point opts))

    (when (get-in opts [:settings :debug :distance])
      (draw-distance! point opts))

    (when (get-in opts [:settings :debug :coordinates])
      (draw-coordinate! point opts))

    (when (get-in opts [:settings :debug :index])
      (draw-index! index point opts))))


(defn draw-clearance! [points opts]
  (doseq [[curr-point tested-points] points
          [in-path? point] tested-points]
    (quil/stroke-weight (:weight opts))
    (if in-path?
      (quil/stroke 255 0 0)
      (quil/stroke 0 100 255))
    (quil/line curr-point point)))


(defn draw-state! [state]
  (apply quil/resize-sketch (get-in state [:settings :size]))
  (quil/frame-rate (get-in state [:settings :fps]))
  (quil/color-mode (get-in state [:settings :mode]))
  (apply quil/background (:background state))

  (quil/with-translation [(/ (quil/width) 2), (/ (quil/height) 2)]
    (draw-slice! (:slice state) state)

    (when (get-in state [:settings :debug :curves])
      (doseq [[color points]
              (map vector [[0 100 255] [0 205 0] [255 0 0] [255 255 0] [0 255 255] [255 0 255]]
                   (get-in state [:debug :slices]))]
        (draw-slice! points (assoc state :color color))))

    (when (get-in state [:settings :debug :clearance])
      (draw-clearance! (get-in state [:debug :clearance]) state))))
