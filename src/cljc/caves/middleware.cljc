(ns caves.middleware
  (:require [quil.core :as quil]
            [gil.core :as gil]
            [clojure.pprint :as pprint]))


(defn mw! [k f] #(update % k juxt f))


(defn show-state! [state font]
  (let [info (with-out-str (pprint/pprint state))]
    (quil/text-font font)
    (when (and (get-in state [:settings :debug :state])
               (seq info))
      (apply quil/fill (:color state))
      (quil/text info 25 25))))


(defn record-gif! [name frames fps _]
  (gil/save-animation (str name ".gif") frames (int (/ 100 fps))))


(defn default-position
  "Default position configuration: zoom is neutral and central point is
  width/2, height/2."
  []
  {:position [(/ (quil/width) 2.0)
              (/ (quil/height) 2.0)]
   :zoom     1})


(defn default-settings
  []
  {:enabled true})


(defn setup-2d-nav
  "Custom 'setup' function which creates initial position
  configuration and puts it to the state map."
  [user-setup user-settings]
  (let [initial-state (-> user-settings
                          (select-keys [:position :zoom :enabled])
                          (->> (merge (default-position) (default-settings))))]
    (update-in (user-setup) [:navigation-2d]
               #(merge initial-state %))))


(defn mouse-dragged
  "Changes center of the sketch depending on the last mouse move. Takes
  zoom into account as well."
  [state event]
  (let [dx (- (:p-x event) (:x event))
        dy (- (:p-y event) (:y event))
        zoom (-> state :navigation-2d :zoom)]
    (-> state
        (update-in [:navigation-2d :position 0] + (/ dx zoom))
        (update-in [:navigation-2d :position 1] + (/ dy zoom)))))


(defn mouse-wheel
  "Changes zoom settings based on scroll."
  [state event]
  (update-in state [:navigation-2d :zoom] * (+ 1 (* -0.1 event))))


(defn draw
  "Calls user draw function with necessary all transformations (position
  and zoom) applied."
  [user-draw state]
  (quil/push-matrix)
  (let [nav-2d (:navigation-2d state)
        zoom (:zoom nav-2d)
        pos (:position nav-2d)]
    (quil/scale zoom)
    (quil/with-translation [(- (/ (quil/width) 2 zoom) (first pos))
                         (- (/ (quil/height) 2 zoom) (second pos))]
                        (user-draw state)))
  (quil/pop-matrix))


(defn navigation-2d
  "Enables navigation over 2D sketch. Dragging mouse will move center of the
  skecth and mouse wheel controls zoom."
  [options]
  (let [user-settings (:navigation-2d options)
        user-draw (:draw options (fn [state]))
        user-mouse-dragged (:mouse-dragged options (fn [state _] state))
        user-mouse-clicked (:mouse-clicked options (fn [state _] state))
        user-mouse-wheel (:mouse-wheel options (fn [state _] state))
        setup (:setup options (fn [] {}))]
    (assoc options
      :setup (partial setup-2d-nav setup user-settings)
      :draw (partial draw user-draw)
      :mouse-dragged (fn [state event]
                       (if (and (not (and (quil/key-pressed?) (:control (quil/key-modifiers))))
                                (get-in state [:navigation-2d :enabled]))
                         (mouse-dragged state event)
                         (user-mouse-dragged state event)))
      :mouse-clicked  (fn [state event]
                       (if (and (get-in state [:navigation-2d :enabled])
                                (not (= :center (:button event))))
                         state
                         (user-mouse-clicked state event)))
      :mouse-wheel (fn [state event]
                     (if (get-in state [:navigation-2d :enabled])
                       (mouse-wheel state event)
                       (user-mouse-wheel state event))))))
