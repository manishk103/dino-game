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
  {:x      (+ 500 (rand-int 100))
   :y      160
   :width  10
   :height 40})

(defn draw-dino [{:keys [x y width height]}]
  (q/rect x y width height)
  (q/fill 0 0 255))

(defn draw-obstacle [{:keys [x y width height]}]
  (q/rect x y width height)
  (q/fill 0))

(defn draw-game-end [{:keys [x y width height]}]
  (q/rect x y width height)
  (q/fill 255 0 0))

(defn make-game []
  {:dino (make-dino)
   :obstacle (make-obstacle)
   :score 0})

(defn jump [dino]
  (if (:jumping? dino)
    dino
    (-> dino
        (update-in [:y] - 100)
        (assoc :jumping? true))))

(defn apply-gravity [dino]
  (if (:jumping? dino)
    (let [falling-dino (update-in dino [:y] + (min 3 (- 100 (:y dino))))]
      (if (= 100 (:y falling-dino))
        (assoc falling-dino :jumping? false)
        falling-dino))
    dino))

(defn move-obstacle [obstacle]
    (update-in obstacle [:x] - 4))

(defn contains-point? [{:keys [x y width height]} [a b]]
  (and (>= a x)
       (<= a (+ x width))
       (>= b y)
       (<= b (+ y height))))

(defn check-colliding? [game-obj-1 game-obj-2]
  (or (contains-point? game-obj-2 [(:x game-obj-1) (:y game-obj-1)])
      (contains-point? game-obj-2 [(:x game-obj-1) (+ (:y game-obj-1) (:height game-obj-1))])
      (contains-point? game-obj-2 [(+ (:x game-obj-1) (:width game-obj-1)) (:y game-obj-1)])
      (contains-point? game-obj-2 [(+ (:x game-obj-1) (:width game-obj-1)) (+ (:y game-obj-1) (:height game-obj-1))])

      (contains-point? game-obj-1 [(:x game-obj-2) (:y game-obj-2)])
      (contains-point? game-obj-1 [(:x game-obj-2) (+ (:y game-obj-2) (:height game-obj-2))])
      (contains-point? game-obj-1 [(+ (:x game-obj-2) (:width game-obj-2)) (:y game-obj-2)])
      (contains-point? game-obj-1 [(+ (:x game-obj-2) (:width game-obj-2)) (+ (:y game-obj-2) (:height game-obj-2))])))

;; Quil
;; ====
(defn setup []
  (q/frame-rate 60)
  (make-game))

(defn update-state [state]
  (if (check-colliding? (:dino state) (:obstacle state))
    ;(q/text (str "Score: " (:score state)) 250 70)
    (make-game)
    (if (<= (get-in state [:obstacle :x]) 0)
      (-> state
          (update-in [:dino] apply-gravity)
          (assoc     :obstacle (make-obstacle)))
      (-> state
          (update-in [:dino] apply-gravity)
          (update-in [:obstacle] move-obstacle)
          (update-in [:score] inc  )))))

(defn draw-state [state]
  (if (check-colliding? (:dino state) (:obstacle state))
    (q/background 255 0 0)
    (do
      (q/background 200)
      (q/text (str "Score: " (:score state)) 500 10)
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
