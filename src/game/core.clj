(ns game.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; Helpers
;; =======
(defn make-dino []
  {:x      10
   :y      100
   :width  30
   :height 100
   :jumping? false})

(defn draw-dino [{:keys [x y width height]}]
  (q/rect x y width height))

(defn make-game []
  {:dino (make-dino)})

(defn jump [dino]
  (if (:jumping? dino)
    dino
    (-> dino
        (update-in [:y] - 80)
        (assoc :jumping? true))))

(defn apply-gravity [dino]
  (if (:jumping? dino)
    (let [falling-dino (update-in dino [:y] + (min 3 (- 100 (:y dino))))]
      (if (= 100 (:y falling-dino))
        (assoc falling-dino :jumping? false)
        falling-dino))
    dino))


;; Quil
;; ====
(defn setup []
  (q/frame-rate 60)
  (make-game))

(defn update-state [state]
  (update-in state [:dino] apply-gravity))

(defn draw-state [state]
  (q/background 200)
  (draw-dino (:dino state)))

(defn key-typed [state {:keys [key-code]}]
  (condp = key-code
    0    (update-in state [:dino] jump)))

(q/defsketch game
  :title "Dino Game"
  :size [600 205]
  :setup setup
  :update update-state
  :draw draw-state
  :key-typed key-typed
  :features [:keep-on-top]
  :middleware [m/fun-mode])
