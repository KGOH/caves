(ns caves.middleware
  (:require [quil.core :as quil]
            [gil.core :as gil]
            [clojure.pprint :as pprint]))


(defn mw! [k f] #(update % k juxt f))


(defn show-fps! [{{:keys [debug]} :settings, :as state} font]
  (when (:fps debug)
    (quil/text-font font)
    (apply quil/fill (:color state))
    (quil/text (str (quil/current-frame-rate)) 25 25)))


(defn show-state! [{{:keys [debug]} :settings, :as state} font]
  (when (:state debug)
    (let [info (with-out-str (pprint/pprint state))]
      (quil/text-font font)
      (apply quil/fill (:color state))
      (quil/text info 25 50))))


(defn record-gif! [name frames fps _]
  (gil/save-animation (str name ".gif") frames (int (/ 100 fps))))
