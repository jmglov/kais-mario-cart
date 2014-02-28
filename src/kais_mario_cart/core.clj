(ns kais-mario-cart.core
  (:require [clojure.java.io :as io])
  (:import (java.awt Color Dimension)
           (javax.swing JFrame JOptionPane JPanel Timer)
           (java.awt.event ActionListener KeyEvent KeyListener)))

(def debug-fn
  "A function taking arguments [world sprite]"
  (atom nil))

(defn sprite-info
  [_ sprite]
  (map sprite [:y :orientation]))

(defn sprite
  "Read a BufferedImage from a file"
  [file]
  (javax.imageio.ImageIO/read (io/input-stream (io/resource file))))

(defn world
  []
  (let [bg-image (sprite "kais_mario_cart/world-before-door.png")
        jack-left-image (sprite "kais_mario_cart/jack-left.png")
        jack-right-image (sprite "kais_mario_cart/jack-right.png")]
    (atom {:background {:image bg-image, :y 0, :x 0}
           :sprites {:jack {:image jack-left-image
                            :orientation :left
                            :y (- (.getWidth bg-image) (.getWidth jack-left-image))
                            :x (- (.getHeight bg-image) (.getHeight jack-left-image))}}
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
  (let [y-modifier (if (= :left direction) - +)
        distance 25]
    (fn [cur-jack]
      (-> (update-in cur-jack [:y] #(y-modifier % distance))
          (assoc-in [:orientation] direction)
          (assoc-in [:image] (jack-image-for direction world))))))

(defn move-jack!
  [panel world direction]
  (let [jack (get-in @world [:sprites :jack])]
    (->>
     (update-sprite world jack (move-jack-fn direction world))
     #(do (when @debug-fn (@debug-fn world %)) %)
     (move! panel world :jack))))

(defn on-key
  [world]
  (fn [keycode panel]
    (cond
     (= keycode KeyEvent/VK_ESCAPE) (System/exit 0)
     (= keycode KeyEvent/VK_LEFT) (move-jack! panel world :left)
     (= keycode KeyEvent/VK_RIGHT) (move-jack! panel world :right))))

(defn -main
  []
  (let [world (world)
        p (panel :on-paint (draw! world) :on-key (on-key world))
        t (Timer. 15 p)]
    (reset! debug-fn sprite-info)
    (show-panel! (frame) p)))
