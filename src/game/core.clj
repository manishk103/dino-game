(ns game.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; Helpers
;; =======
(defn make-dino []
  {:x      60
   :y      100
   :width  20
   :height 100
   :jumping? false})

(defn make-obstacle []
  {:x      (+ 500 (rand-int 100))
   :y      (+ 142 (rand-int 50))
   :width  10
   :height 100})

(defn draw-dino [{:keys [x y width height]}]
  (q/fill 0)
  (q/rect x y width height))

(defn draw-obstacle [{:keys [x y width height]}]
  (q/fill 0 0 255)
  (q/rect x y width height))

(defn make-game []
  {:dino (make-dino)
   :obstacles [(make-obstacle)]
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

(defn move-obstacles [obstacles]
  (for [obstacle obstacles]
    (update-in obstacle [:x] - 4)))

(defn contains-point? [{:keys [x y width height]} [a b]]
  (and (>= a x)
       (<= a (+ x width))
       (>= b y)
       (<= b (+ y height))))

(defn check-colliding? [game-obj-1 game-obj-2]
  (if-not (and game-obj-1 game-obj-2)
    false
    (or (contains-point? game-obj-2 [(:x game-obj-1) (:y game-obj-1)])
        (contains-point? game-obj-2 [(:x game-obj-1) (+ (:y game-obj-1) (:height game-obj-1))])
        (contains-point? game-obj-2 [(+ (:x game-obj-1) (:width game-obj-1)) (:y game-obj-1)])
        (contains-point? game-obj-2 [(+ (:x game-obj-1) (:width game-obj-1)) (+ (:y game-obj-1) (:height game-obj-1))])

        (contains-point? game-obj-1 [(:x game-obj-2) (:y game-obj-2)])
        (contains-point? game-obj-1 [(:x game-obj-2) (+ (:y game-obj-2) (:height game-obj-2))])
        (contains-point? game-obj-1 [(+ (:x game-obj-2) (:width game-obj-2)) (:y game-obj-2)])
        (contains-point? game-obj-1 [(+ (:x game-obj-2) (:width game-obj-2)) (+ (:y game-obj-2) (:height game-obj-2))]))))

;; Quil
;; ====
(defn setup []
  (q/frame-rate 60)
  (make-game))

(defn update-state [state]
  (if (check-colliding? (:dino state)
                        (-> state :obstacles first))
    (make-game)
    (let [state (if (and (-> state :obstacles first)
                         (= (-> state :obstacles first :x)
                            300))
                  (update-in state [:obstacles] conj (make-obstacle))
                  state)
          state (-> state
                    (update-in [:dino] apply-gravity)
                    (update-in [:obstacles] move-obstacles))]
      (if (and (-> state :obstacles first)
               (<= (-> state :obstacles first :x) 10))
        (update-in state [:obstacles] rest)
        (update-in state [:score] inc)))))

(defn draw-state [state]
  (if (check-colliding? (:dino state)
                        (first (:obstacles state)))
    (q/background 255 0 0)
    (do
      (q/background 200)
      (q/text (str "Score: " (:score state)) 500 10)
      (draw-dino (:dino state))
      (doseq [obstacle (:obstacles state)]
        (draw-obstacle obstacle)))))

(defn key-typed [state {:keys [key-code]}]
  (condp = key-code
    0    (update-in state [:dino] jump)
    state))

(q/defsketch game
  :title "Dino Game"
  :size [600 205]
  :setup setup
  :update update-state
  :draw draw-state
  :key-typed key-typed
  :features [:keep-on-top]
  :middleware [m/fun-mode])
