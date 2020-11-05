(ns caves.math
  (:require [quil.core :as quil]
            [clojure.core.matrix.random :as matrix.random]))


(defn normal-rand
  ([] (first (matrix.random/sample-normal 1)))
  ([b] (* b (quil/abs (first (matrix.random/sample-normal 1)))))
  ([a b] (+ a (normal-rand (- b a)))))


(defn rand-num [a b]
  (+ a (rand (- b a))))


(defn random-decision [prob]
  (>= prob (rand-num 0 1)))


(defn degree->radian [degree]
  (* quil/DEG-TO-RAD degree))


(defn constrain
  [a b x]
  (-> x (max a) (min b)))


(defn distance [& points]
  (apply quil/dist (apply concat points)))


(defn polar->cartesian [{:keys [radius eccentricity angle]
                         :or   {radius 1, eccentricity 0}}]
  (let [radius**2    (quil/pow radius 2)
        e**2         (quil/pow eccentricity 2)
        minor-radius (quil/sqrt (- radius**2 (* radius**2 e**2)))]
    (if (neg? eccentricity)
      [(* minor-radius (quil/cos angle))
       (* radius (quil/sin angle))]
      [(* radius (quil/cos angle))
       (* minor-radius (quil/sin angle))])))


(defn cartesian->polar [[x y]]
  {:radius (quil/sqrt (+ (quil/pow x 2) (quil/pow y 2)))
   :angle  (quil/atan2 y x)})


(defn rotate [angle [q q+s]]
  (-> (mapv - q+s q)
      cartesian->polar
      (update :angle + angle)
      polar->cartesian
      (->> (mapv + q)
           (vector q))))


(defn direction [[q q+s]]
  (-> (mapv - q+s q)
      cartesian->polar
      (assoc :radius 1)
      polar->cartesian))


(defn middle-of-angle [p2 p1 p3]
  (let [a          (map - p2 p1)
        b          (map - p3 p1)
        a-angle    (apply quil/atan2 (reverse a))
        b-angle    (apply quil/atan2 (reverse b))
        angle      (- (cond-> b-angle
                        (> b-angle a-angle)
                        (+ quil/TWO-PI))
                      a-angle)
        half-angle (/ angle 2)]
    (direction (rotate half-angle [p1 p2]))))


(defn point-crosses-segment? [[x y] [[x1 y1] [x2 y2]]]
  (and (not= (< y y1) (< y y2))
       (< x (-> (- x2 x1)
                (* (- y y1))
                (/ (- y2 y1))
                (+ x1)))))

(defn path-contains-point? [path point]
  (->> (map vector path (rest (cycle path)))
       (map (partial point-crosses-segment? point))
       (filter true?)
       count
       odd?))


(defn segment-contains?
  "Checks if segment [a, b) contains x"
  [[a b] x]
  (when (and a b x)
    (let [a'  (mod a quil/TWO-PI)
          b'  (mod b quil/TWO-PI)
          x'  (mod x quil/TWO-PI)
          b'' (cond-> b' (< b' a') (+ quil/TWO-PI))]
      (and (<= a' x') (< x' b'')))))


(defn angle-diff [a1 a2]
  (-> (quil/atan2 (quil/sin (- a1 a2))
                  (quil/cos (- a1 a2)))
      quil/abs))


(defn cross* [[x1 y1] [x2 y2]]
  (- (* x1 y2) (* y1 x2)))


(defn segment&ray-intersection [[q q+s] [p theta]]
  (let [s     (mapv - q+s q)
        r     (polar->cartesian {:angle theta})
        q-p   (mapv - q p)
        r*s   (cross* r s)

        #_#_q-p*r (cross* q-p r)
        q-p*s (cross* q-p s)
        t     (/ q-p*s r*s)
        #_#_u     (/ q-p*r r*s)]
    (when (and (not= 0 r*s)
               #_(<= 0 t 1)
               #_(<= 0 u 1))
      (mapv + p (mapv (partial * t) r)))))


(defn angles-too-close? [approx angle1 angle2]
  (< approx (angle-diff angle1 angle2)))


(defn diff-is-almost-zero? [approx a b]
  (> (- (max a b)
        (min a b))
     approx))
