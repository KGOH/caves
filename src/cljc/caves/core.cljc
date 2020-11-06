(ns caves.core
  (:require [caves.math :as math]
            [caves.slice-generator :as slice-generator]
            [caves.draw :as draw]
            [caves.middleware :as mw]
            [quil.core :as quil]
            [quil.middleware :as quil.mw]))


(def default-state
  {:approx       (/ quil/TWO-PI 31) ;; number here must be greater than max points count
   :radius       400
   :eccentricity {:value     0.8
                  :deviation 0.01
                  :limit     0.8
                  :approx    0.00001}
   :clearance    {:radius [15 35] :angle (quil/radians 30)}
   :curves       [{:deviation 50, :points-count 9}
                  {:deviation 10, :points-count 30}]
   :formations   [{:height      [20 90] ;; Stalactites ;; TODO: Stalagnates
                   :width       [15 40]
                   :distance    35
                   :max-count   9
                   :direction   [0 1]
                   :probability 0.2
                   :rule        (fn [[x y]] (and (> -75 y) (> 200 (quil/abs x))))}
                  {:height      [20 90] ;; Stalagmites
                   :width       [15 40]
                   :distance    35
                   :max-count   9
                   :direction   [0 -1]
                   :probability 0.1
                   :rule        (fn [[x y]] (and (< 75 y) (> 200 (quil/abs x))))}]
   :background   [0]
   :color        [255]
   :weight       3
   :settings     {:title "Caves"
                  :size  [1000 1000]
                  :fps   1
                  :mode  :rgb
                  :debug #{#_:reset  #_:pause #_:state
                           #_:curves #_:lines #_:clearance
                           #_:points #_:index #_:distance #_:coordinates}}})


(defn update-state [state]
  (as-> state $
    (merge $ (select-keys default-state [:settings]))
    (assoc $ :fps (quil/current-frame-rate))
    (cond-> $
      (get-in $ [:settings :debug :pause])
      (assoc-in [:settings :fps] 10)

      (not (get-in $ [:settings :debug :pause]))
      (as-> $
          (merge $ (slice-generator/generate-slice $))
        (update-in $ [:eccentricity :deviation]
                   (if (math/diff-is-almost-zero?
                        (:approx (:eccentricity $))
                        (:limit (:eccentricity $))
                        (quil/abs (:value (:eccentricity $))))
                     identity
                     -))
        (update-in $ [:eccentricity :value]
                   + (:limit (:eccentricity $))
                   (math/rand-num (- (:deviation (:eccentricity $)))
                                  (:deviation (:eccentricity $))))
        (update-in $ [:eccentricity :value]
                   (partial math/constrain (- (:limit (:eccentricity $)))
                            (:limit (:eccentricity $)))))

      (get-in $ [:settings :debug :reset])
      (merge default-state))))


(defn draw-state! [state]
  (apply quil/resize-sketch (get-in state [:settings :size]))
  (quil/frame-rate (get-in state [:settings :fps]))
  (quil/color-mode (get-in state [:settings :mode]))
  (apply quil/background (:background state))

  (quil/with-translation [(/ (quil/width) 2), (/ (quil/height) 2)]
    (draw/draw-slice! (:slice state) state)

    (when (get-in state [:settings :debug :curves])
      (doseq [[color points]
              (map vector [[0 100 255] [0 205 0] [255 0 0] [255 255 0] [0 255 255] [255 0 255]]
                   (get-in state [:debug :slices]))]
        (draw/draw-slice! points (assoc state :color color))))

    (when (get-in state [:settings :debug :clearance])
      (draw/draw-clearance! (get-in state [:debug :clearance]) state))))


(defn -main [& args]
  (quil/defsketch caves
    :title      (get-in default-state [:settings :title])
    :size       (get-in default-state [:settings :size])
    :setup      #(update-state default-state)
    :update     update-state
    :draw       draw-state!
    :middleware [quil.mw/fun-mode
                 mw/navigation-2d
                 (mw/mw! :draw #(mw/show-state! (dissoc % :slice :debug) (quil/create-font "Iosevka Regular" 20)))
                 (mw/mw! :draw (partial mw/record-gif! "caves" 20 1))]))
