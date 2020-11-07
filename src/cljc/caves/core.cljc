(ns caves.core
  (:require [caves.math :as math]
            [caves.slice-generator :as slice-generator]
            [caves.draw :as draw]
            [caves.middleware :as mw]
            [quil.core :as quil]
            [quil.middleware :as quil.mw]
            [clojure.core.matrix :as matrix]))


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
   :last-update  0
   :settings     {:render    :2d
                  :title     "Caves"
                  :size      [1000 1000]
                  :ups       1
                  :fps       60
                  :mode      :rgb
                  :debug     #{#_:reset  #_:pause #_:state     #_:fps
                               #_:curves #_:lines #_:clearance
                               #_:points #_:index #_:distance  #_:coordinates}}})


(defn new-eccentricity [{:keys [value deviation approx limit]}]
  (let [limit-reached? (math/diff-is-almost-zero? approx limit (quil/abs value))
        new-deviation  (cond-> deviation limit-reached? -)]
    {:approx    approx
     :limit     limit
     :deviation new-deviation
     :value     (->> [(- new-deviation) new-deviation]
                     sort
                     (apply math/rand-num)
                     (+ new-deviation value)
                     (math/constrain (- limit) limit))}))


(defn update-state [{{:keys [debug] :as settings} :settings, :as state}]
  (cond-> state
    (and (not (:pause debug))
         (<= (/ 1000 (:ups settings)) (- (quil/millis) (:last-update state))))
    (-> (assoc :eccentricity (new-eccentricity (:eccentricity state)))
        (merge (slice-generator/generate-slice state))
        (assoc-in [:last-update] (quil/millis)))

    (and (not (:pause debug))
         (= :3d (:render settings)))
    (update :slice #(matrix/set-column % 2 (repeat (matrix/dimension-count % 0) 0)))

    (:reset debug)
    (merge default-state)

    :always
    (assoc-in [:settings] (:settings default-state))))


(defn draw-state! [{{:keys [debug render]} :settings, :as state}]
  (apply quil/resize-sketch (get-in state [:settings :size]))
  (quil/frame-rate (get-in state [:settings :fps]))
  (quil/color-mode (get-in state [:settings :mode]))
  (apply quil/background (:background state))

  (quil/with-translation (if (= :3d render)
                           [(/ (quil/width) 2), (/ (quil/height) 2), 0]
                           [(/ (quil/width) 2), (/ (quil/height) 2)])
    (draw/draw-slice! (:slice state) state)

    (when (:curves debug)
      (doseq [[color points]
              (map vector [[0 100 255] [0 205 0] [255 0 0] [255 255 0] [0 255 255] [255 0 255]]
                   (get-in state [:debug :slices]))]
        (draw/draw-slice! points (assoc state :color color))))

    (when (:clearance debug)
      (draw/draw-clearance! (get-in state [:debug :clearance]) state))))


(defn -main [& args]
  (let [settings (get-in default-state [:settings])]
    (quil/defsketch caves
      :title      (:title settings)
      :size       (:size settings)
      :setup      #(update-state default-state)
      :update     update-state
      :draw       draw-state!
      :renderer   (if (= :3d (:render settings)) :p3d :java2d)
      :middleware (cond-> [quil.mw/fun-mode]

                    (= :3d (:render settings))
                    (conj quil.mw/navigation-3d)

                    (= :2d (:render settings))
                    (conj (mw/mw! :draw #(mw/show-fps! % (quil/create-font "Iosevka" 20)))
                          quil.mw/navigation-2d
                          (mw/mw! :draw #(mw/show-state! (dissoc % :slice :debug) (quil/create-font "Iosevka" 20)))
                          (mw/mw! :draw (partial mw/record-gif! "caves" 20 1)))))))
