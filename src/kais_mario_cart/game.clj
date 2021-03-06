(ns kais-mario-cart.game
  (:require [kais-mario-cart.core :as kmc :refer [deflevel defelement defcontrol]]
            [kais-mario-cart.debug :as debug])
  (:import java.awt.event.KeyEvent))

(defn on-stairs?
  [stairs]
  (fn [world sprite]
    (let [slope (/ (- (:b stairs) (:t stairs)) (- (:r stairs) (:l stairs)))
          [l r t b] (kmc/bounding-box sprite)]
      (kmc/colliding? sprite [120 130 890 893]))))

(defn stairs [w h]
;; [stairs {:l 100, :r 250, :t 575, :b 893}
;;    stairs (merge stairs {:on? (on-stairs? stairs)})]
;; 
;;   :move {:up (fn [sprite]
;;                (-> sprite
;;                    (update-in [:y] #(+ % 25))
;;                    (update-in [:x] #(- % 25))))
;;          :down (fn [sprite]
;;                  )}  )
  )

(defn world
  [width height]
  (let [bg-image (kmc/image "kais_mario_cart/world-before-door.png")
        jack-left-image (kmc/image "kais_mario_cart/jack-left.png")
        jack-right-image (kmc/image "kais_mario_cart/jack-right.png")]
    (atom {:width width, :height height
           :background {:image bg-image, :y 0, :x 0}
           :sprites {:jack {:image jack-left-image
                            :orientation :left
                            :y (- (.getWidth bg-image) (.getWidth jack-left-image))
                            :x (- (.getHeight bg-image) (.getHeight jack-left-image))}}
           :terrain {:stairs (stairs width height)}
           :images {:background bg-image
                    :jack-left jack-left-image
                    :jack-right jack-right-image}})))

(defn jack-image-for
  [direction world]
  (get-in @world (conj [:images] (keyword (str "jack-" (name direction))))))

(defn move-jack-fn
  [direction world]
  (let [distance 25]
    (fn [cur-jack]
      (cond
       (= :up direction) (if (on-stairs? cur-jack)
                           ((-> @world :terrain :stairs :move :up) cur-jack)
                           cur-jack)
       (= :down direction) cur-jack
       :else (-> (update-in cur-jack [:y] #((kmc/direction->fn direction) % distance))
                 (assoc-in [:orientation] direction)
                 (assoc-in [:image] (jack-image-for direction world)))))))

(defn move-jack!
  [panel world direction]
  (let [jack (get-in @world [:sprites :jack])]
    (->>
     (kmc/update-sprite world jack (move-jack-fn direction world))
     (debug/debug-sprite world)
     (kmc/move! panel world :jack))))

(defn on-key
  [world]
  (fn [keycode panel]
    (let [direction (kmc/keycode->direction keycode)]
      (cond
       (kmc/is-action? :quit keycode) (System/exit 0)
       ((comp not nil?) direction) (move-jack! panel world direction)))))

(defn img-path [img-name]
  (str "kais_mario_cart/" img-name ".png"))

(deflevel 1 :image (img-path "world-before-door") :on-victory [])
(defelement jack :image (img-path "jack-left") :x 1095 :y 627)
(defcontrol move-jack-left KeyEvent/VK_LEFT
  (kmc/move jack :x -50))

(defn -main
  [& args]
  (debug/activate-from-args! args)
  (kmc/show-panel! (kmc/make-frame "Kai's Mario Cart") (kmc/make-panel)))
