(ns caves.two-dimensional
  (:require [caves.math :as math]
            [caves.slice-generator :as slice-generator]
            [caves.draw :as draw]
            [caves.middleware :as mw]
            [quil.core :as quil]
            [quil.middleware :as quil.mw]
            [clojure.core.matrix :as matrix]))


(def default-state
  {:approx          0.20268340264597245 #_(/ quil/TWO-PI 31) ;; number here must be greater than max points count
   :radius          400
   :lerp-steps      5
   :eccentricity    {:value     0.8
                     :deviation 0.01
                     :limit     0.8
                     :approx    0.00001}
   :clearance       {:radius [15 35] :angle 0.5235988 #_(quil/radians 30)}
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
   :weight          3
   :settings        {:title  "Caves"
                     :size   [1000 1000]
                     :font   "Iosevka",
                     :ups    1
                     :fps    60
                     :mode   :rgb
                     :debug  #{#_:reset  #_:pause #_:state     #_:fps
                               #_:curves #_:lines #_:clearance
                               #_:points #_:index #_:distance  #_:coordinates}}
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
    (-> (merge (slice-generator/generate state))
        (as-> $ (assoc $ :slices (:with-formations $)))
        (assoc :eccentricity (new-eccentricity (:eccentricity state)))
        (assoc-in [:last-update] (quil/millis)))

    (:reset debug)
    (merge default-state)

    :always
    (assoc-in [:settings] (:settings default-state))))


(defn setup! []
  #_(apply quil/resize-sketch (get-in default-state [:settings :size]))
  (quil/frame-rate (get-in default-state [:settings :fps]))
  (quil/color-mode (get-in default-state [:settings :mode]))
  default-state)


(defn draw-state! [{{:keys [debug render]} :settings, :as state}]
  (apply quil/background (:background state))
  (quil/with-translation [(/ (quil/width) 2), (/ (quil/height) 2)]

    (draw/draw-slice! (:slices state) state)

    (when (:curves debug)
      (doseq [[color points]
              (map vector [[0 100 255] [0 205 0] [255 0 0] [255 255 0] [0 255 255] [255 0 255]]
                   (get-in state [:debug :slices]))]
        (draw/draw-slice! points (assoc state :color color))))

    (when (:clearance debug)
      (draw/draw-clearance! (get-in state [:debug :clearance]) state))))


(defn ^:export run-sketch []
  (let [settings (get-in default-state [:settings])]
    (quil/defsketch caves2d
      :host       "caves"
      :title      (:title settings)
      :size       (:size settings)
      :setup      setup!
      :update     update-state
      :draw       draw-state!
      :middleware (cond-> [quil.mw/fun-mode
                           (mw/mw! :draw #(mw/show-fps! % #?(:cljs (:font settings), :clj (quil/create-font (:font settings) 10))))
                           (mw/mw! :draw #(mw/show-state! (dissoc % :slice :debug :slices :walls :formations) #?(:cljs (:font settings), :clj (quil/create-font (:font settings) 20))))
                           quil.mw/navigation-2d
                           #_(mw/mw! :draw (partial mw/record-gif! "caves" 20 1))]))))


(defn -main [& args]
  (run-sketch))
