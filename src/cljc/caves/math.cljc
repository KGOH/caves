(ns caves.math
  (:require [quil.core :as quil]
            [clojure.core.matrix.random :as matrix.random]))


(defn normal-rand []
  (first (matrix.random/sample-normal 1)))


(defn rand-num [a b]
  (+ a (rand (- b a))))


(defn degree->radian [degree]
  (* quil/DEG-TO-RAD degree))


(defn constrain
  [a b x]
  (-> x (max a) (min b)))


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
   :angle  (quil/atan (/ y x))})


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

        #_#_q-p*r (cross* q-p r)
        q-p*s (cross* q-p s)
        t     (/ q-p*s r*s)
        #_#_u     (/ q-p*r r*s)]
    (when (and (not= 0 r*s)
               #_(<= 0 t 1)
               #_(<= 0 u 1))
      (map + p (map (partial * t) r)))))


(defn angles-too-close? [approx angle1 angle2]
  (< approx (angle-diff angle1 angle2)))


(defn diff-is-almost-zero? [approx a b]
  (> (- (max a b)
        (min a b))
     approx))
