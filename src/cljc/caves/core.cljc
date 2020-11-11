(ns caves.core
  (:require [caves.math :as math]
            [caves.slice-generator :as slice-generator]
            [caves.draw :as draw]
            [caves.two-dimensional :as two-dimensional]

            [quil.core :as quil :include-macros true]
            [quil.middleware :as quil.mw]
            [clojure.core.matrix :as matrix]))


(def Z 2)
(def Y 1)
(def X 0)

(def get-z #(get % Z 0))
(def get-y #(get % Y 0))
(def get-x #(get % X 0))


(def default-state
  {:approx          0.20268340264597245 #_(/ quil/TWO-PI 31) ;; number here must be greater than max points count
   :radius          400
   :lerp-steps      5
   :render-steps    50
   :slice-distance  100
   :boundary        10000000
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
   :weight          2
   :settings        {:title  "Caves"
                     :size   [1000 1000]
                     :font   "Iosevka",
                     :ups    1
                     :fps    60
                     :mode   :rgb
                     :debug  #{#_:reset #_:state #_:fps #_:lines}}
   :navigation-3d   {:position  [0 0 0]
                     :straight  [0 0 100]
                     :up        [0 1 0]
                     :step-size 5}})


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


(defn generate [{:keys [slices walls render-steps lerp-steps slice-distance] :as state}]
  (let [z-coord          (-> state :navigation-3d :position get-z)
        visible-distance (* (+ lerp-steps render-steps) slice-distance)
        get-slice-z-pos  (comp get-z first)
        behind?          (partial > z-coord)
        visible?         #(<= (- z-coord visible-distance) % (+ z-coord visible-distance))
        invisible?       (complement visible?)

        visible-slices   (->> slices
                              (drop-while (comp invisible? get-slice-z-pos))
                              (take-while (comp visible? get-slice-z-pos)))
        [behind ahead]   (split-with (comp behind? get-slice-z-pos) visible-slices)
        amount-to-gen    (-> render-steps
                             (- (count ahead))
                             (/ (inc lerp-steps))
                             quil/ceil)
        last-generated   (or (some->> (last walls) (hash-map :walls))
                             (slice-generator/generate state))
        last-z           (-> ahead last first (get Z z-coord))
        new-slices       (repeatedly amount-to-gen #(slice-generator/generate state))
        interpolated     (->> new-slices
                              (cons last-generated)
                              (partition 2 1)
                              (mapcat
                               (fn [[{wall1 :walls} {wall2 :walls, formations2 :with-formations}]]
                                 (conj (mapv (partial slice-generator/interpolate wall1 wall2)
                                             (rest (range 0 1 (/ 1 lerp-steps))))
                                       formations2)))
                              (map-indexed
                               (fn [i points]
                                 (->> i
                                      inc
                                      (* slice-distance)
                                      (+ last-z)
                                      repeat
                                      (mapv conj points)))))]
    (cond-> {:slices (into (vec visible-slices) (vec interpolated))}
      (seq new-slices) (-> (assoc :walls (mapv :walls new-slices)) ;; TODO: moving back will cause loss of last wall
                           (assoc :with-formations (mapv :with-formations new-slices))))))


(defn move-slice [distance slice]
  (matrix/add slice (repeat (matrix/dimension-count slice X) [0 0 distance])))


(defn teleport [state]
  (let [z-coord (-> state :navigation-3d :position get-z)]
    (when (> (quil/abs z-coord) (:boundary state))
      (-> state
          (update :slices (partial mapv (partial move-slice (- z-coord))))
          (update-in [:navigation-3d :position Z] - z-coord)
          (update-in [:navigation-3d :straight Z] - z-coord)))))


(defn update-state [{{:keys [debug] :as settings} :settings, :as state}]
  (apply quil/camera (mapcat (:navigation-3d state) [:position :straight :up]))
  (-> state
      (update-in [:navigation-3d :position Z] + (get-in state [:navigation-3d :step-size]))
      (update-in [:navigation-3d :straight Z] + (get-in state [:navigation-3d :step-size]))
      (merge (generate state))
      (merge (teleport state))
      (assoc-in [:settings] (:settings default-state))
      (cond-> (:reset debug) (merge default-state))))


(defn setup! []
  #_(apply quil/resize-sketch (get-in default-state [:settings :size]))
  #_(quil/frame-rate (get-in default-state [:settings :fps]))
  (quil/color-mode (get-in default-state [:settings :mode]))
  default-state)


(defn draw-state! [state]
  (apply quil/background (:background state))
  (let [slices (:slices state)
        fov {:current-pos (-> state :navigation-3d :position get-z)
             :distance    (* (:render-steps state) (:slice-distance state))
             :straight    (-> state :navigation-3d :straight get-z)}]
    (doseq [slice slices]
      (draw/draw-slice! slice (assoc state :fov fov)))))


(defn ^:export run-sketch []
  (let [settings (get-in default-state [:settings])]
    (quil/defsketch caves
      :host       "caves"
      :title      (:title settings)
      :size       (:size settings)
      :setup      setup!
      :update     update-state
      :draw       draw-state!
      :renderer   :p3d
      #_#_:navigation-3d {:position  [0 0 0]
                      :straight  [0 0 100]
                      :up        [0 1 0]
                      :step-size 100}
      :middleware (cond-> [quil.mw/fun-mode
                           quil.mw/navigation-3d]))))


(defn -main [& args]
  (run-sketch))
