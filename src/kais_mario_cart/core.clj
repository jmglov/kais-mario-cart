(ns kais-mario-cart.core
  (:require [clojure.java.io :as io]
            [clojure.set :refer [intersection]])
  (:import (java.awt Color Dimension)
           (javax.swing JFrame JOptionPane JPanel Timer)
           (java.awt.event ActionListener KeyEvent KeyListener)))

(def debug-fn
  "A function taking arguments [world sprite]"
  (atom nil))

(defn sprite-info
  [_ sprite]
  (println (map sprite [:y :x :orientation])))

(defn image
  "Reads a BufferedImage from a file"
  [file]
  (javax.imageio.ImageIO/read (io/input-stream (io/resource file))))

(defn bounding-box
  "Returns the bounding box for a sprite as its left and right y values and bottom and
   top x values as [l r b t]"
  [sprite]
  (let [{:keys [y x]} sprite
        image (:image sprite)
        l y
        r (+ y (.getWidth image))
        b x
        t (+ x (.getHeight image))]
    [l r b t]))

(defn intersect?
  "Returns true if there is an intersection between the set of numbers starting at s1 and
   ending at s2 (exclusive at the end) and the set starting at s2 and ending at e2."
  [[s1 e1] [s2 e2]]
  ((comp not empty?) (intersection (set (range s1 e1)) (set (range s2 e2)))))

(defn colliding?
  "Returns true if there is a collision between a sprite and the bounding box defined by
   left and right y values (l and r) and bottom and top x values (b and t)"
  [sprite [l r b t]]
  (let [[sl sr sb st] (bounding-box sprite)]
    (and (intersect? [sl sr] [l r]) (intersect? [sb st] [b t]))))

(defn on-stairs?
  [sprite]
  (let [[l r t b] (bounding-box sprite)]
    (colliding? sprite [120 130 890 893])))

(defn world
  []
  (let [bg-image (image "kais_mario_cart/world-before-door.png")
        jack-left-image (image "kais_mario_cart/jack-left.png")
        jack-right-image (image "kais_mario_cart/jack-right.png")]
    (atom {:background {:image bg-image, :y 0, :x 0}
           :sprites {:jack {:image jack-left-image
                            :orientation :left
                            :y (- (.getWidth bg-image) (.getWidth jack-left-image))
                            :x (- (.getHeight bg-image) (.getHeight jack-left-image))}}
           :terrain {:stairs {:dy 25, :dx 25
                              :on? on-stairs?}}
           :images {:background bg-image
                    :jack-left jack-left-image
                    :jack-right jack-right-image}})))

(defn frame
  []
  (JFrame. "Kai's Mario Cart"))

(defn panel
  [& {:keys [on-paint on-action on-key width height]
      :or {width 1263, height 893}}]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (when on-paint
        (on-paint g this)))
    (actionPerformed [e]
      (when on-action
        (on-action e this))
      (.repaint this))
    (keyPressed [e]
      (when on-key
        (on-key (.getKeyCode e) this)))
    (getPreferredSize []
      (Dimension. width height))
    (keyReleased [e])
    (keyTyped [e])))

(defn show-panel!
  [frame panel]
  (doto panel
    (.setFocusable true)
    (.addKeyListener panel))
  (doto frame
    (.add panel)
    (.pack)
    (.setVisible true)))

(defn draw-image!
  ([graphics widget image x y]
     (-> graphics (.drawImage image x y widget)))
  ([widget image x y]
     (-> widget .getGraphics (draw-image! widget image x y))))

(defn draw!
  [world]
  (fn [graphics widget]
    (apply draw-image! graphics widget (map (:background @world) [:image :y :x]))
    (doseq [s (-> @world :sprites vals)]
      (apply draw-image! graphics widget (map s [:image :y :x])))))

(defn move!
  [panel world id sprite]
  (reset! world (update-in @world [:sprites id] sprite))
  (.repaint panel))

(defn sprite-y-limits [sprite world]
  (let [world-width (-> @world :background :image .getWidth)
        sprite-width (-> sprite :image .getWidth)]
    [0 (- world-width sprite-width)]))

(defn direction->fn
  [direction]
  (cond
   (#{:up :right} direction) +
   :else -))

(defn update-sprite [world sprite move-fn]
  (let [new-sprite (move-fn sprite)
        y (:y new-sprite)
        [l-lim r-lim] (sprite-y-limits sprite world)]
    (cond
     (< y l-lim) (assoc-in new-sprite [:y] l-lim)
     (> y r-lim) (assoc-in new-sprite [:y] r-lim)
     :else new-sprite)))

(defn jack-image-for
  [direction world]
  (get-in @world (conj [:images] (keyword (str "jack-" (name direction))))))

(defn move-jack-fn
  [direction world]
  (let [distance 25]
    (fn [cur-jack]
      (cond
       (= :up direction) cur-jack
       (= :down direction) cur-jack
       :else (-> (update-in cur-jack [:y] #((direction->fn direction) % distance))
                 (assoc-in [:orientation] direction)
                 (assoc-in [:image] (jack-image-for direction world)))))))

(defn move-jack!
  [panel world direction]
  (let [jack (get-in @world [:sprites :jack])]
    (->>
     (update-sprite world jack (move-jack-fn direction world))
     #(do (when @debug-fn (@debug-fn world %)) %)
     (move! panel world :jack))))

(def keycode->direction {KeyEvent/VK_LEFT :left
                         KeyEvent/VK_RIGHT :right
                         KeyEvent/VK_UP :up
                         KeyEvent/VK_DOWN :down})

(defn on-key
  [world]
  (fn [keycode panel]
    (let [direction (keycode->direction keycode)]
      (cond
       (= keycode KeyEvent/VK_ESCAPE) (System/exit 0)
       ((comp not nil?) direction) (move-jack! panel world direction)))))

(defn -main
  []
  (let [world (world)
        p (panel :on-paint (draw! world) :on-key (on-key world))
        t (Timer. 15 p)]
    (reset! debug-fn sprite-info)
    (show-panel! (frame) p)))
