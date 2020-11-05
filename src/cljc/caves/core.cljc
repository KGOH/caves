(ns caves.core
  (:require [quil.core :as quil]
            [quil.middleware :as quil.mw]
            [caves.math :as math]
            [caves.draw :as draw]
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


(defn add-formation [points {:keys [rule max-count probability direction height width distance]}]
  (loop [[prev-point point next-point & rrest-points :as points]
         (cons nil points)

         processed-points         []
         spawned-formations-count 0]
    (cond
      (or (>= spawned-formations-count max-count)
          (nil? point))
      (concat processed-points (rest points))

      (and (rule point) (math/random-decision probability))
      (let [formation-center (->> (mapv * direction (repeat (gen-random-deviation height)))
                                  (mapv + point))
            [formation-left-base formation-right-base]
            (->> [(->> (mapv * (reverse direction) (repeat (gen-random-deviation width)))
                       (mapv - point))
                  (->> (mapv * (reverse direction) (repeat (gen-random-deviation width)))
                       (mapv + point))]
                 (sort-by (partial math/distance prev-point)))

            prev-too-close?    (>= distance (math/distance prev-point formation-left-base))
            prev-base-too-far? (< quil/HALF-PI
                                  (math/angle-diff (math/angle [1 0] [0 0] (math/direction [point prev-point]))
                                                   (math/angle [1 0] [0 0] (math/direction [formation-left-base prev-point]))))

            next-too-close?    (>= distance (math/distance next-point formation-right-base))
            next-base-too-far? (< quil/HALF-PI
                                  (math/angle-diff (math/angle [1 0] [0 0] (math/direction [point next-point]))
                                                   (math/angle [1 0] [0 0] (math/direction [formation-right-base next-point]))))

            formation (->> [(when-not (or prev-base-too-far? prev-too-close?) formation-left-base)
                            formation-center
                            (if-not (or next-base-too-far? next-too-close?) formation-right-base next-point)]
                           (remove nil?))]
        (recur (if (or next-base-too-far? next-too-close?)
                 rrest-points
                 (rest points))
               (into processed-points formation)
               (inc spawned-formations-count)))

      :else (recur (rest points)
                   (conj processed-points point)
                   spawned-formations-count))))


(defn gen-points-to-test [{:keys [angle radius]} [_ base-point _ :as points]]
  (let [[radius-small radius-big] radius
        middle-of-angle           (apply math/middle-of-angle points)
        left-clearance-border     (second (math/rotate (- (/ angle 2)) [[0 0] middle-of-angle]))
        right-clearance-border    (second (math/rotate (/ angle 2) [[0 0] middle-of-angle]))
        radius-scalars            (mapv (partial repeat 2) [radius-small radius-big radius-small])]
    (->> [left-clearance-border middle-of-angle right-clearance-border]
         (mapv (partial mapv *) radius-scalars)
         (mapv (partial mapv + base-point)))))


(defn fix-self-inersecions [clearance curve]
  (loop [[prev-point curr-point next-point & rest-points :as points]
         (concat [(last curve)] curve [(first curve)])

         result        []
         tested-points []]
    (if (nil? next-point)
      {:result        result
       :tested-points tested-points}
      (let [angle  [prev-point curr-point next-point]
            tested (mapv (juxt (partial math/path-contains-point? (concat result points))
                               identity)
                         (gen-points-to-test clearance angle))]
        (if (every? (comp true? first) tested)
          (recur (rest points)
                 (conj result curr-point)
                 (conj tested-points [curr-point tested]))
          (recur (cons prev-point (cons next-point rest-points))
                 result
                 (conj tested-points [curr-point tested])))))))

(defn generate-slice [{:keys [approx radius clearance eccentricity curves formations]} & [seed]]
  (let [[main-curve & curves]
        (map #(generate-curve (assoc % :radius radius) seed) curves)

        points
        (->> (populate-groups (make-groups main-curve) (apply concat curves))
             (sort-by ffirst)
             vals
             (mapcat (partial project-points-on-segment approx))
             (sort-by :angle)
             (map (comp math/polar->cartesian (partial merge {:eccentricity eccentricity}))))
        with-formations (reduce add-formation points formations)
        {fixed :result, :keys [tested-points]} (fix-self-inersecions clearance with-formations)]
    {:slice fixed
     :debug {:clearance tested-points
             :slices    (concat [with-formations points]
                                (map (->> (partial merge {:eccentricity eccentricity})
                                       (comp math/polar->cartesian)
                                       (partial map))
                                  (cons main-curve curves)))}}))

(def default-state
  {:approx                 (/ quil/TWO-PI 31) ;; number here must be greater than max points count
   :radius                 400
   :eccentricity           0.8
   :eccentricity-deviation 0.01
   :eccentricity-limit     0.8
   :eccentricity-approx    0.00001
   :clearance              {:radius [15 35] :angle (* quil/DEG-TO-RAD 30)}
   :curves                 [{:deviation 50, :points-count 9}
                            {:deviation 10, :points-count 30}]
   :formations             [{:height      [40 90] ;; Stalactites ;; TODO: Stalagnates
                             :width       [15 40]
                             :distance    35
                             :max-count   9
                             :direction   [0 1]
                             :probability 0.2
                             :rule        (fn [[x y]] (and (> -75 y) (> 200 (quil/abs x))))}
                            {:height      [40 90] ;; Stalagmites
                             :width       [15 40]
                             :distance    35
                             :max-count   9
                             :direction   [0 -1]
                             :probability 0.1
                             :rule        (fn [[x y]] (and (< 75 y) (> 200 (quil/abs x))))}]
   :background [0]
   :color      [255]
   :weight     3
   :settings   {:title "Caves"
                :size  [1000 1000]
                :fps   1
                :mode  :rgb
                :debug #{#_:reset  #_:pause #_:state
                         #_:curves #_:lines #_:clearance
                         #_:points #_:index #_:distance #_:coordinates}}})

(defn update-state [state]
  (as-> state $
    (merge $ (select-keys default-state [:settings]))
    (cond-> $
      (get-in $ [:settings :debug :pause])
      (assoc-in [:settings :fps] 10)

      (not (get-in $ [:settings :debug :pause]))
      (as-> $
        (merge $ (generate-slice $))
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


(defn -main [& args]
  (quil/defsketch caves
    :title      (get-in default-state [:settings :title])
    :size       (get-in default-state [:settings :size])
    :setup      #(update-state default-state)
    :update     update-state
    :draw       draw/draw-state!
    :middleware [quil.mw/fun-mode
                 mw/navigation-2d
                 (mw/mw! :draw #(mw/show-state! (dissoc % :slice) (quil/create-font "Iosevka Regular" 20)))
                 (mw/mw! :draw (partial mw/record-gif! "caves" 20 1))]))
