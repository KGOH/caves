(ns caves.core
  (:require [quil.core :as quil]
            [quil.middleware :as quil.mw]
            [clojure.string :as str]
            [clojure.core.matrix.random :as matrix.random]))


(defn degree->radian [degree]
  (* quil/DEG-TO-RAD degree))


(def rad360 (degree->radian 360))


(defn constrain
  [a b x]
  (-> x (max a) (min b)))


(defn polar->cartesian [{:keys [r e angle]
                         :or {r 1, e [0 0]}}]
  (let [[ex ey] e]
    [(* (+ r ex) (quil/cos angle))
     (* (+ r ey) (quil/sin angle))]))


(defn cartesian->polar [[x y]]
  {:r (quil/sqrt (+ (quil/pow x 2) (quil/pow y 2)))
   :angle (quil/atan (/ y x))})


(defn normal-rand []
  (first (matrix.random/sample-normal 1)))


(defn segment-contains?
  "Checks if segment [a, b) contains x"
  [[a b] x]
  (when (and a b x)
    (let [b (if (< b a) ##Inf b)] ;; TODO: better way compare segments of circle
      (and (<= a x) (< x b)))))

(defn angle-diff [a1 a2]
  (-> (quil/atan2 (quil/sin (- a1 a2))
                  (quil/cos (- a1 a2)))
      quil/abs))


(defn cross* [[x1 y1] [x2 y2]]
  (- (* x1 y2) (* y1 x2)))


(defn segment&ray-intersection [[q q+s] [p theta]]
  (let [s     (map - q+s q)
        r     (polar->cartesian {:angle theta})
        q-p   (map - q p)
        r*s   (cross* r s)

        q-p*r (cross* q-p r)
        q-p*s (cross* q-p s)
        t     (/ q-p*s r*s)
        u     (/ q-p*r r*s)]
    (when (and (not= 0 r*s)
               #_(<= 0 t 1)
               #_(<= 0 u 1))
      (map + p (map (partial * t) r)))))


(defn generate-slice [{:keys [approx e r] :as state} & [seed]]
  (let [[main-curve & curves]
        (for [{:keys [points-cnt d]} (:curves state)]
          (for [angle (range 0 rad360 (/ quil/TWO-PI points-cnt))]
            (let [d' (-> (normal-rand) quil/abs (* d) int -)]
              {:r (+ r d'), :d d', :max-d d :angle angle})))
        groups (as-> main-curve $
                 (partition 2 1 [(first main-curve)] $)
                 (map (partial zipmap [:start :end]) $))
        curve  (->> (reduce (fn [acc {:keys [angle] :as pt}]
                              (update-in acc
                                         [(->> (keys acc)
                                               (filter #(segment-contains? % angle))
                                               first)
                                          :points]
                                         (fnil conj [])
                                         pt))
                            (->> groups
                                 (interleave (map (juxt (comp :angle :start) (comp :angle :end))
                                                  groups))
                                 (apply hash-map))
                            (apply concat curves))
                    vals
                    (sort-by (comp :angle :start))
                    (mapcat (fn [{:keys [start end points]}]
                              (->> points
                                   (keep (fn [{:keys [d max-d angle] :as p}]
                                           (when (and (< approx (angle-diff angle (:angle start)))
                                                      (< approx (angle-diff angle (:angle end))))
                                             (let [x (-> (segment&ray-intersection
                                                          [(polar->cartesian start)
                                                           (polar->cartesian end)]
                                                          [[0 0] angle])
                                                         cartesian->polar)]
                                               (assoc p :r (+ (:r x) (/ max-d 2) d))))))
                                   (cons start))))
                    (map (partial merge {:e [e (- e)]}))
                    (sort-by :angle))]
    (map (partial map polar->cartesian)
         #_[curve]
         (cons curve (cons main-curve curves)))))


(def default-state
  {:fps         5
   :mode        :rgb
   :background  [0]
   :color       [255]
   :approx      (degree->radian 10)
   :r           300
   :weight      4
   :e           30
   :e-d         2.5
   :e-lim       30
   :debug       false
   :debug-state false
   :curves      [{:d 30, :points-cnt 9}
                 {:d 10, :points-cnt 30}
                 {:d 50, :points-cnt 7}]})


(defn update-state [state]
  (as-> state $
    (assoc $ :points (generate-slice $))
    (update $ :e-d (if (< (quil/abs (:e $)) (:e-lim $))
                     identity
                     -))
    (update $ :e + (:e-d $) (->> (rand (* 2 (:e-d $)))
                                 (- (:e-d $))))
    (update $ :e (partial constrain (- (:e-lim $)) (:e-lim $)))
    (merge $ default-state)))


(defn setup []
  (update-state default-state))


(defn draw-state [state]
  (quil/frame-rate (:fps state))
  (quil/color-mode (:mode state))
  (apply quil/background (:background state))
  (apply quil/fill   (:color state))
  (apply quil/stroke (:color state))
  (quil/stroke-weight (:weight state))
  (quil/with-translation [(/ (quil/width) 2), (/ (quil/height) 2)]
    (doseq [[color points] (->> (cond-> (:points state)
                                  (not (:debug state))
                                  (->> (take 1)))
                                (map vector
                                     [[255] [0 100 255] [0 205 0] [255 0 0]])
                                reverse)]
      (apply quil/fill   color)
      (apply quil/stroke color)
      (when (:debug state)
        (do
          (quil/stroke-weight 10)
          (doseq [[x y] points]
            (quil/ellipse x y (:weight state) (:weight state)))))
      #_(doseq [[p1 p2] (partition 2 1 points points)]
          (quil/line p1 p2))
      (do
        (quil/stroke-weight (:weight state))
        (quil/no-fill)
        (quil/begin-shape)
        (doseq [p (take (+ 3 (count points))
                        (cycle points))]
          (apply quil/curve-vertex p))
        (quil/end-shape))
      #_(do
          (quil/no-fill)
          (doseq [pts (partition 4 3 points points)]
            (apply quil/bezier (apply concat pts)))))))


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
