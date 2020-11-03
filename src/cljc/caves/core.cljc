(ns caves.core
  (:require [quil.core :as quil]
            [quil.middleware :as quil.mw]
            [caves.math :as math]
            [clojure.string :as str]))


(defn make-groups [main-curve]
  (as-> main-curve $
    (partition 2 1 [(first main-curve)] $)
    (map (partial zipmap [:start :end]) $)
    (interleave (map (juxt (comp :angle :start) (comp :angle :end))
                     $)
                $)
    (apply hash-map $)))


(defn collect-group-points [groups curves]
  (reduce (fn [acc {:keys [angle] :as pt}]
            (update-in acc
                       [(->> (keys acc)
                             (filter #(math/segment-contains? % angle))
                             first)
                        :points]
                       (fnil conj [])
                       pt))
          groups
          curves))


(defn angles-too-close? [approx angle1 angle2]
  (< approx (math/angle-diff angle1 angle2)))


(defn project-points-on-segment [filtering-approx {:keys [start end points]}]
  (->> points
       (keep (fn [{:keys [d max-d angle] :as p}]
               (when (every? (partial angles-too-close? filtering-approx angle)
                             [(:angle start) (:angle end)])
                 (let [x (-> (math/segment&ray-intersection
                              [(math/polar->cartesian start)
                               (math/polar->cartesian end)]
                              [[0 0] angle])
                             math/cartesian->polar)]
                   (assoc p :radius (+ (:radius x) (/ max-d 2) d))))))
       (cons start)))


(defn generate-curve [{:keys [points-cnt d]} radius & [seed]]
  (for [angle (range 0 quil/TWO-PI (/ quil/TWO-PI points-cnt))
        :let  [d' (-> (math/normal-rand) quil/abs (* d) int -)]]
    {:radius (+ radius #_d')
     :d     0#_d'
     :max-d 0#_d
     :angle angle}))


(defn generate-slice [{:keys [approx radius e curves]} & [seed]]
  (let [[main-curve & curves] (map #(generate-curve % radius seed) curves)

        curve
        (->> (collect-group-points (make-groups main-curve)
                                   (apply concat curves))
             vals
             (sort-by (comp :angle :start))
             (mapcat (partial project-points-on-segment approx))
             (sort-by :angle))]
    (map (partial map (comp math/polar->cartesian (partial merge {:e [e (- e)]})))
         (concat [curve main-curve] curves))))


(def default-state
  {:fps         5
   :mode        :rgb
   :debug       true
   :debug-state false
   :background  [0]
   :color       [255]
   :approx      (math/degree->radian 10)
   :weight      4
   :radius      300
   :e           30
   :e-d         2.5
   :e-lim       30
   :curves      [{:d 30, :points-cnt 9}
                 {:d 10, :points-cnt 30}
                 #_{:d 50, :points-cnt 7}]})


(defn update-state [state]
  (as-> state $
    (assoc $ :points (generate-slice $))
    (update $ :e-d (if (< (quil/abs (:e $)) (:e-lim $))
                     identity
                     -))
    (update $ :e + (:e-d $) (->> (rand (* 2 (:e-d $)))
                                 (- (:e-d $))))
    (update $ :e (partial math/constrain (- (:e-lim $)) (:e-lim $)))
    (merge $ default-state)))


(defn setup []
  (update-state default-state))


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
      (quil/text info 15 15))))


(defn mw! [k f] #(update % k juxt f))


(defn -main [& args]
  (quil/defsketch caves
    :size       [800 800]
    :setup      setup
    :update     update-state
    :draw       draw-state
    :features   [:keep-on-top]
    :middleware [quil.mw/fun-mode (mw! :draw show-info)]))
