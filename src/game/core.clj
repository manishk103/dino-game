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
  {:x      600
   :y      (+ 142 (rand-int 50))
   :width  10
   :height 100})

(defn draw-dino [{:keys [x y width height]}]
  (q/rect x y width height))

(defn draw-obstacle [{:keys [x y width height]}]
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
  (vec
   (for [obstacle obstacles]
    (update-in obstacle [:x] - 5))))

(defn contains-point? [{:keys [x y width height]} [a b]]
  (and (>= a x)
       (<= a (+ x width))
       (>= b y)
       (<= b (+ y height))))

(defn corners-of [game-obj]
 [[(:x game-obj) (:y game-obj)]
  [(:x game-obj) (+ (:y game-obj) (:height game-obj))]
  [(+ (:x game-obj) (:width game-obj)) (:y game-obj)]
  [(+ (:x game-obj) (:width game-obj)) (+ (:y game-obj) (:height game-obj))]])


(defn check-colliding? [game-obj-1 game-obj-2]
  (if-not (and game-obj-1 game-obj-2)
    false
    (->> (concat (map #(contains-point? game-obj-1 %)
                     (corners-of game-obj-2))
                 (map #(contains-point? game-obj-2 %)
                     (corners-of game-obj-1)))
         (reduce #(or %1 %2)))))

(defn add-new-obstacle [state]
  (if (and (let [o (-> state :obstacles last)]
             (if o
               (< 100 (:x o))
               true))
           (<= (rand-int 1000) 20))
    (update-in state [:obstacles] conj (make-obstacle))
    state))


;; Quil
;; ====
(defn setup []
  (q/frame-rate 60)
  (make-game))

(defn update-state [state]
  (if (check-colliding? (:dino state) (-> state :obstacles first))
    (make-game)
    (let [state (add-new-obstacle state)
          state (-> state
                    (update-in [:dino] apply-gravity)
                    (update-in [:obstacles] move-obstacles))]
      (if (and (-> state :obstacles first)
               (<= (-> state :obstacles first :x) 10))
        (update-in state [:obstacles] (comp vec rest))
        (update-in state [:score] inc)))))

(defn draw-state [state]
  (if (check-colliding? (:dino state)
                        (-> state :obstacles first))
    (q/background 255 0 0)
    (do
      (q/background 200)

      (q/fill 0)
      (q/text (str "Score: " (:score state)) 400 10)
      (q/text (str "No. of obstacles: " (-> state :obstacles count)) 400 20)

      (draw-dino (:dino state))

      (q/fill 0 0 255)
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
