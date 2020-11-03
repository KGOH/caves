(ns caves.core
  (:require [quil.core :as quil]
            [quil.middleware :as quil.mw]
            [caves.math :as math]
            [clojure.string :as str]))


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


(defn generate-curve [{:keys [radius points-count deviation]} & [seed]]
  (for [angle (range 0 quil/TWO-PI (/ quil/TWO-PI points-count))
        :let  [rand-deviation (-> (math/normal-rand) quil/abs (* deviation) int -)]]
    {:radius        (+ radius rand-deviation)
     :deviation     rand-deviation
     :max-deviation deviation
     :angle         angle}))


(defn generate-slice [{:keys [approx radius eccentricity curves]} & [seed]]
  (let [[main-curve & curves]
        (map #(generate-curve (assoc % :radius radius) seed) curves)

        points
        (->> (populate-groups (make-groups main-curve) (apply concat curves))
             (sort-by ffirst)
             vals
             (mapcat (partial project-points-on-segment approx))
             (sort-by :angle))]
    (map (partial map (comp math/polar->cartesian (partial merge {:eccentricity eccentricity})))
         (concat [points main-curve] curves))))


(def default-state
  {:fps                    5
   :mode                   :rgb
   :debug                  false
   :debug-state            false
   :background             [0]
   :color                  [255]
   :approx                 (/ quil/TWO-PI 35) ;; number here mast be greater than max points count
   :weight                 4
   :radius                 300
   :eccentricity           0.5
   :eccentricity-deviation 0.01
   :eccentricity-limit     0.8
   :eccentricity-approx    0.00001
   :curves                 [{:deviation 50, :points-count 9}
                            {:deviation 10, :points-count 30}]})


(defn update-state [state]
  (as-> state $
    #_(merge $ default-state)
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
                     (:eccentricity-limit $)))))


(defn draw-state [state]
  (quil/frame-rate (:fps state))
  (quil/color-mode (:mode state))
  (apply quil/background (:background state))

  (quil/with-translation [(/ (quil/width) 2), (/ (quil/height) 2)]
    (doseq [[color points]
            (->> (cond-> (:points state)
                   (not (:debug state))
                   (->> (take 1)))
                 (map vector
                      [(:color state) [0 100 255] [0 205 0] [255 0 0]])
                 reverse)]

      (apply quil/fill   color)
      (apply quil/stroke color)

      (when (:debug state)
        (quil/stroke-weight 10)
        (doseq [[x y] points]
          (quil/ellipse x y (:weight state) (:weight state))))

      (quil/stroke-weight (:weight state))
      (quil/no-fill)
      (quil/begin-shape)
      (doseq [p (take (+ 3 (count points))
                      (cycle points))]
        (apply quil/curve-vertex p))
      (quil/end-shape))))


(defn show-info [state]
  (let [info (dissoc state :points)
        info (str/join \newline (mapv (partial str/join ": ") info))]
    (quil/text-font (quil/create-font "Iosevka Regular" 20) 20)
    (when (and (:debug-state state) (seq info))
      (apply quil/fill (:color state))
      (quil/text info 25 25))))


(defn mw! [k f] #(update % k juxt f))


(defn -main [& args]
  (quil/defsketch caves
    :size       [800 800]
    :setup      #(update-state default-state)
    :update     update-state
    :draw       draw-state
    :middleware [quil.mw/fun-mode (mw! :draw show-info)]))
