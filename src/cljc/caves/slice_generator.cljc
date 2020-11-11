(ns caves.slice-generator
  (:require [caves.math :as math]
            [quil.core :as quil]
            [clojure.core.matrix :as matrix]))


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


(defn distance-ok? [point zero-point neighboring-point distance] ;; TODO: still not pefrect
  (and (< distance (math/distance point neighboring-point))
       (> quil/HALF-PI
          (math/angle-diff (math/angle [1 0] [0 0] (math/direction [zero-point neighboring-point]))
                           (math/angle [1 0] [0 0] (math/direction [point neighboring-point]))))))


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
      (let [height-deviation (matrix/mul direction (repeat 2 (gen-random-deviation height)))
            width-deviation  (matrix/mul (reverse direction) (repeat 2 (gen-random-deviation width)))

            formation-center (matrix/add point height-deviation)
            [formation-left-base formation-right-base]
            (->> [(matrix/sub point width-deviation)
                  (matrix/add point width-deviation)]
                 (sort-by (partial math/distance prev-point)))

            prev-base-ok-distance? (distance-ok? formation-left-base point prev-point distance)
            next-base-ok-distance? (distance-ok? formation-right-base point next-point distance)

            formation (->> [(when prev-base-ok-distance? formation-left-base)
                            formation-center
                            (if next-base-ok-distance? formation-right-base next-point)]
                           (remove nil?))]
        (recur (if next-base-ok-distance?
                 (rest points)
                 rrest-points)
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
         (mapv matrix/mul radius-scalars)
         (mapv (partial matrix/add base-point)))))


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


(defn generate [{:keys [approx radius clearance eccentricity curves formations]} & [seed]]
  (let [[main-curve & curves]
        (map #(generate-curve (assoc % :radius radius) seed) curves)

        points
        (->> (populate-groups (make-groups main-curve) (apply concat curves))
             (sort-by ffirst)
             vals
             (mapcat (partial project-points-on-segment approx))
             (sort-by :angle)
             (map (comp math/polar->cartesian (partial merge {:eccentricity (:value eccentricity)}))))
        with-formations (reduce add-formation points formations)
        {fixed :result, :keys [tested-points]} (fix-self-inersecions clearance with-formations)]
    {:with-formations fixed
     :walls           points
     :debug           {:clearance tested-points
                       :slices    (concat [with-formations points]
                                          (map (->> (partial merge {:eccentricity (:value eccentricity)})
                                                    (comp math/polar->cartesian)
                                                    (partial map))
                                               (cons main-curve curves)))}}))


(defn quadpol [[x1 y1] [x2 y2] [x3 y3] x]
  (+ (* y1 (/ (* (- x x2) (- x x3)) (* (- x1 x2) (- x1 x3))))
     (* y2 (/ (* (- x x1) (- x x3)) (* (- x2 x1) (- x2 x3))))
     (* y3 (/ (* (- x x1) (- x x2)) (* (- x3 x1) (- x3 x2))))))


(defn interpolate [start end amount]
  (mapv (partial mapv (fn [x y] (quil/lerp x y amount)))
        start
        end))
