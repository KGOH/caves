(ns caves.core
  (:require [quil.core :as quil]
            [quil.middleware :as quil.mw]
            [caves.math :as math]
            [caves.middleware :as mw]))


(defn make-groups
  [curve]
  (as-> curve $
    (partition 2 1 [(first curve)] $)
    (map (partial zipmap [:start :end]) $)
    (interleave (map (juxt (comp :angle :start) (comp :angle :end))
                     $)
                $)
    (apply hash-map $)))


(defn populate-groups [groups points]
  (reduce (fn [acc {:keys [angle] :as pt}]
            (update-in acc
                       [(->> (keys acc)
                             (filter #(math/segment-contains? % angle))
                             first)
                        :points]
                       (fnil conj [])
                       pt))
          groups
          points))


(defn project-points-on-segment [filtering-approx {:keys [start end points]}]
  (->> points
       (keep (fn [{:keys [deviation max-deviation angle] :as p}]
               (when (every? (partial math/angles-too-close? filtering-approx angle)
                             [(:angle start) (:angle end)])
                 (let [x (-> (math/segment&ray-intersection
                              [(math/polar->cartesian start)
                               (math/polar->cartesian end)]
                              [[0 0] angle])
                             math/cartesian->polar)]
                   (assoc p :radius (+ (:radius x) (/ max-deviation 2) deviation))))))
       (cons start)))


(defn gen-random-deviation [deviation & [seed]]
  (-> (apply math/normal-rand deviation) int))


(defn generate-curve [{:keys [radius points-count deviation]} & [seed]]
  (for [angle (range 0 quil/TWO-PI (/ quil/TWO-PI points-count))
        :let  [rand-deviation (- (gen-random-deviation [deviation] seed))]]
    {:radius        (+ radius rand-deviation)
     :deviation     rand-deviation
     :max-deviation deviation
     :angle         angle}))


(defn add-formation [points {:keys [rule max-count probability direction height width]}]
  (loop [[point & rest-points :as points] points
         processed-points                 []
         spawned-formations-count         0]
    (cond
      (or (>= spawned-formations-count max-count)
          (nil? point))
      (concat processed-points points)

      (and (rule point) (math/normal-random-decision probability))
      (recur rest-points
             (concat processed-points
                     [(->> (mapv * (reverse direction) (repeat (gen-random-deviation width)))
                           (mapv - point))
                      (->> (mapv * direction (repeat (gen-random-deviation height)))
                           (mapv + point))
                      (->> (mapv * (reverse direction) (repeat (gen-random-deviation width)))
                           (mapv + point))])
             (inc spawned-formations-count))

      :else (recur rest-points
                   (concat processed-points [point])
                   spawned-formations-count))))


(defn fix-self-inersecions [clearance curve]
  (->> (map vector (cycle (cons (last curve) (butlast curve))) curve (rest (cycle curve)))
       (keep (fn [[_ p _ :as points]]
               (when (->> (apply math/middle-of-angle points)
                          (mapv (partial * clearance))
                          (mapv + p)
                          (math/path-contains-point? curve))
                 p)))))


(defn generate-slice [{:keys [approx radius clearance eccentricity curves formations]} & [seed]]
  (let [[main-curve & curves]
        (map #(generate-curve (assoc % :radius radius) seed) curves)

        points
        (->> (populate-groups (make-groups main-curve) (apply concat curves))
             (sort-by ffirst)
             vals
             (mapcat (partial project-points-on-segment approx))
             (sort-by :angle)
             (map (comp math/polar->cartesian (partial merge {:eccentricity eccentricity}))))]
    (cons (->> (reduce add-formation points formations)
               (fix-self-inersecions clearance))
          (map (partial map (comp math/polar->cartesian (partial merge {:eccentricity eccentricity})))
               (cons main-curve curves)))))


(def default-state
  {:approx                 (/ quil/TWO-PI 31) ;; number here must be greater than max points count
   :radius                 400
   :eccentricity           0.5
   :eccentricity-deviation 0.01
   :eccentricity-limit     0.8
   :eccentricity-approx    0.00001
   :clearance              50
   :curves                 [{:deviation 50, :points-count 9}
                            {:deviation 10, :points-count 30}]
   :formations             [{:height      [20 90]
                             :width       [20 40]
                             :max-count   9
                             :direction   [0 1]
                             :probability 0.2
                             :rule        (fn [[x y]] (and (> 75 y) (> 200 (quil/abs x))))}
                            {:height      [60 90]
                             :width       [20 20]
                             :max-count   9
                             :direction   [0 -1]
                             :probability 0.2
                             :rule        (fn [[x y]] (and (< -75 y) (> 200 (quil/abs x))))}]
   :background [0]
   :color      [255]
   :weight     3
   :settings   {:title "Caves"
                :size  [1000 1000]
                :fps   5
                :mode  :rgb
                :debug {:reset  false
                        :pause  false
                        :curves false
                        :points false
                        :lines  false
                        :state  false}}})


(defn update-state [state]
  (as-> state $
    (merge $ (select-keys default-state [:settings]))
    (cond-> $
      (not (get-in $ [:settings :debug :pause]))
      (as-> $
        (assoc $ :points (generate-slice $))
        (update $ :eccentricity-deviation
                (if (math/diff-is-almost-zero?
                     (:eccentricity-approx state)
                     (:eccentricity-limit state)
                     (quil/abs (:eccentricity state)))
                  identity
                  -))
        (update $ :eccentricity
                + (:eccentricity-deviation $)
                (math/rand-num (- (:eccentricity-deviation $))
                               (:eccentricity-deviation $)))
        (update $ :eccentricity
                (partial math/constrain (- (:eccentricity-limit $))
                         (:eccentricity-limit $))))

      (get-in $ [:settings :debug :reset])
      (merge default-state))))


(defn draw-state! [state]
  (apply quil/resize-sketch (get-in state [:settings :size]))
  (quil/frame-rate (get-in state [:settings :fps]))
  (quil/color-mode (get-in state [:settings :mode]))
  (apply quil/background (:background state))

  (quil/with-translation [(/ (quil/width) 2), (/ (quil/height) 2)]
    (doseq [[color points]
            (->> (cond-> (:points state)
                   (not (get-in state [:settings :debug :curves]))
                   (->> (take 1)))
                 (map vector
                      [(:color state) [0 100 255] [0 205 0] [255 0 0]])
                 reverse)]

      (apply quil/fill   color)
      (apply quil/stroke color)

      (when (get-in state [:settings :debug :points])
        (quil/stroke-weight 10)
        (doseq [[x y] points]
          (quil/ellipse x y (:weight state) (:weight state))))

      (quil/stroke-weight (:weight state))
      (quil/no-fill)

      (if (get-in state [:settings :debug :lines])
        (do
          (quil/begin-shape)
          (doseq [p points]
            (apply quil/vertex p))
          (quil/end-shape :close))
        (do
          (quil/begin-shape)
          (doseq [p (take (+ 3 (count points)) (cycle points))]
            (apply quil/curve-vertex p))
          (quil/end-shape))))))


(defn -main [& args]
  (quil/defsketch caves
    :title      (get-in default-state [:settings :title])
    :size       (get-in default-state [:settings :size])
    :setup      #(update-state default-state)
    :update     update-state
    :draw       draw-state!
    :middleware [quil.mw/fun-mode
                 mw/navigation-2d
                 (mw/mw! :draw #(mw/show-state! (dissoc % :points) (quil/create-font "Iosevka Regular" 20)))
                 (mw/mw! :draw (partial mw/record-gif! "caves" 10 5))]))
