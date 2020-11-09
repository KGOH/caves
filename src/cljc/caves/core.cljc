(ns caves.core
  (:require [caves.math :as math]
            [caves.slice-generator :as slice-generator]
            [caves.draw :as draw]
            [caves.middleware :as mw]
            [quil.core :as quil]
            [quil.middleware :as quil.mw]
            [clojure.core.matrix :as matrix]))


(def default-state
  {:approx          (/ quil/TWO-PI 31) ;; number here must be greater than max points count
   :radius          400
   :lerp-steps      5
   :render-steps    15
   :slice-distance  100
   :eccentricity    {:value     0.8
                     :deviation 0.01
                     :limit     0.8
                     :approx    0.00001}
   :clearance       {:radius [15 35] :angle (quil/radians 30)}
   :curves          [{:deviation 50, :points-count 9}
                     {:deviation 10, :points-count 30}]
   :formations      [{:height      [20 90] ;; Stalactites ;; TODO: Stalagnates
                      :width       [20 40]
                      :distance    35
                      :max-count   9
                      :direction   [0 1]
                      :probability 0.2
                      :rule        (fn [[x y]] (and (> -75 y) (> 200 (quil/abs x))))}
                     {:height      [20 90] ;; Stalagmites
                      :width       [20 40]
                      :distance    35
                      :max-count   9
                      :direction   [0 -1]
                      :probability 0.1
                      :rule        (fn [[x y]] (and (< 75 y) (> 200 (quil/abs x))))}]
   :background      [0]
   :color           [255]
   :weight          1
   :settings        {:title  "Caves"
                     :size   [1000 1000]
                     :font   ["Iosevka" 20]
                     :ups    1
                     :fps    60
                     :mode   :rgb
                     :debug  #{#_:reset #_:pause #_:state #_:fps #_:curves #_:lines #_:disable-shadow}}
   :walls           '()
   :with-formations '()
   :debug           '()})


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
         (<= (/ 1000 (:ups settings 60)) (- (quil/millis) (:last-update state 0))))
    (-> ((partial merge-with conj) (slice-generator/generate state))
        (update :walls             (partial take (:render-steps state)))
        (update :with-formations   (partial take (:render-steps state)))
        (update :debug             (partial take (:render-steps state)))
        (assoc :eccentricity (new-eccentricity (:eccentricity state)))
        (as-> $ (assoc $ :slices (->> (mapcat (fn [wall1 wall2 formation2]
                                                (concat
                                                 (map (partial slice-generator/interpolate wall1 wall2)
                                                      (rest (range 0 1 (/ 1 (:lerp-steps $)))))
                                                 [formation2]))
                                              (:walls $)
                                              (rest (:walls $))
                                              (rest (:with-formations $)))
                                      (cons (first (:with-formations $)))
                                      (map-indexed
                                       (fn [i points]
                                         (matrix/set-column points
                                                            2
                                                            (repeat (matrix/dimension-count points 0)
                                                                    (* (- i
                                                                          (inc (* (:lerp-steps state)
                                                                                  (dec (:render-steps state)))))
                                                                       (:slice-distance $)))))))))
        (assoc-in [:last-update] (quil/millis)))

    (:reset debug)
    (merge default-state)

    :always
    (assoc-in [:settings] (:settings default-state))))


(defn setup! []
  #_(apply quil/resize-sketch (get-in default-state [:settings :size]))
  (quil/camera 200 200 900 0 0 0 0 1 0)
  (quil/frame-rate (get-in default-state [:settings :fps]))
  (quil/color-mode (get-in default-state [:settings :mode]))
  default-state)


(defn draw-state! [state]
  (apply quil/background (:background state))
  (quil/with-translation [(/ (quil/width) 2), (/ (quil/height) 2), 0]
    (doseq [slice (:slices state)]
      (draw/draw-slice! slice state))))


(defn -main [& args]
  (let [settings (get-in default-state [:settings])]
    (quil/defsketch caves
      :title      (:title settings)
      :size       (:size settings)
      :setup      setup!
      :update     update-state
      :draw       draw-state!
      :renderer   :p3d
      :middleware (cond-> [quil.mw/fun-mode
                           (mw/mw! :draw #(mw/show-fps! % (apply quil/create-font (:font settings))))
                           (mw/mw! :draw #(mw/show-state! (dissoc % :slice :debug :slices :walls :formations) (apply quil/create-font (:font settings))))
                           (conj quil.mw/navigation-3d)]))))
