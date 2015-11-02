(ns game.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; Helpers
;; =======
(defn make-dino []
  {:x      60
   :y      100
   :width  30
   :height 100
   :jumping? false})

(defn make-obstacle []
  {:x      (+ 600 (rand-int 100))
   :y      160
   :width  10
   :height 40})

(defn game-end []
  {:x      400
   :y      160
   :width  20
   :height 40})

(defn draw-dino [{:keys [x y width height]}]
  (q/rect x y width height)
  (q/fill 0))

(defn draw-obstacle [{:keys [x y width height]}]
  (q/rect x y width height)
  (q/fill 0))

(defn draw-game-end [{:keys [x y width height]}]
  (q/rect x y width height)
  (q/fill 255 0 0))

(defn make-game []
  {:dino (make-dino)
   :obstacle (make-obstacle)})

(defn jump [dino]
  (if (:jumping? dino)
    dino
    (-> dino
        (update-in [:y] - 90)
        (assoc :jumping? true))))

(defn apply-gravity [dino]
  (if (:jumping? dino)
    (let [falling-dino (update-in dino [:y] + (min 2 (- 100 (:y dino))))]
      (if (= 100 (:y falling-dino))
        (assoc falling-dino :jumping? false)
        falling-dino))
    dino))

(defn move-obstacle [obstacle]
    (update-in obstacle [:x] - 4))

;;(defn check-game-state [dino obstacle]
;;  (if (or (= (get-in dino [:y]) (get-in obstacle [:x])) (= (get-in dino [:y]) (get-in obstacle [:y])))
;;    true))


(defn check-colliding? [dino obstacle]
  (and (and (>= (+ (:x dino) (:width dino)) (:x obstacle))
            (<= (+ (:x dino) (:width dino)) (+ (:x obstacle)
                                               (:width obstacle))))
       (and (>= (+ (:y dino) (:height dino)) (:y obstacle))
            (<= (+ (:y dino) (:height dino)) (+ (:y obstacle)
                                                (:height obstacle))))))

;; Quil
;; ====
(defn setup []
  (q/frame-rate 60)
  (make-game))

(defn update-state [state]
  (if (check-colliding? (:dino state) (:obstacle state))
    (make-game)
    (if (<= (get-in state [:obstacle :x]) 0)
      (-> state
          (update-in [:dino] apply-gravity)
          (assoc      :obstacle (make-obstacle)))
      (-> state
          (update-in [:dino] apply-gravity)
          (update-in [:obstacle] move-obstacle)))))

(defn draw-state [state]
  (if (check-colliding? (:dino state) (:obstacle state))
    (q/background 255 0 0)
    (do
      (q/background 200)
      (draw-dino (:dino state))
      (draw-obstacle (:obstacle state)))))

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
