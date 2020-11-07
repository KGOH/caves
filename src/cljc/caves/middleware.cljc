(ns caves.middleware
  (:require [quil.core :as quil]
            [gil.core :as gil]
            [clojure.pprint :as pprint]))


(defn mw! [k f] #(update % k juxt f))


(defn show-state! [{{:keys [debug]} :settings, :as state} font]
  (let [info (with-out-str (pprint/pprint state))]
    (quil/text-font font)
    (when (:fps debug)
      (apply quil/fill (:color state))
      (quil/text (str (quil/current-frame-rate)) 25 25))
    (when (and (:state debug) (seq info))
      (apply quil/fill (:color state))
      (quil/text info 25 50))))


(defn record-gif! [name frames fps _]
  (gil/save-animation (str name ".gif") frames (int (/ 100 fps))))
