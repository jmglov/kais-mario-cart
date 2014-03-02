(ns kais-mario-cart.core
  (:require [clojure.java.io :as io]
            [clojure.set :refer [intersection]]
            [clojure.string :refer [join split]]
            [kais-mario-cart.util :refer [warn]])
  (:import (java.awt Color Dimension)
           (javax.swing JFrame JOptionPane JPanel Timer)
           (java.awt.event ActionListener KeyEvent KeyListener)))

(def debug-fns
  {:sprite-info (fn [_ sprite] (println (map sprite [:y :x :orientation])))})

(def active-debug-fns (atom []))

(defn activate-debug-fn!
  [fn-key]
  (let [f (debug-fns fn-key)]
    (if f
      (swap! active-debug-fns conj f)
      (warn (str "Debug function " fn-key " not found! Available functions are: "
                 (join " " (map name (keys debug-fns))))))))

(defn debug-fn-keys
  [args]
  (if (args "--debug")
    (split (or (args "--debug") "") #",")
    []))

(defn- debug
  [world sprite retval]
  (doseq [f @active-debug-fns]
    (f world sprite))
  retval)

(defn debug-world
  "Invokes all active debug functions with world and sprite as arguments, then returns world"
  [world sprite]
  (debug world sprite world))

(defn debug-sprite
  "Invokes all active debug functions with world and sprite as arguments, then returns sprite"
  [world sprite]
  (debug world sprite sprite))

(defn image
  "Reads a BufferedImage from a file"
  [file]
  (javax.imageio.ImageIO/read (io/input-stream (io/resource file))))

(defn bounding-box
  "Returns the bounding box for a sprite as its left and right y values and top and
   bottom x values as [l r t b]"
  [sprite]
  (let [{:keys [y x]} sprite
        image (:image sprite)
        l y
        r (+ y (.getWidth image))
        t x
        b (+ x (.getHeight image))]
    [l r t b]))

(defn intersect?
  "Returns true if there is an intersection between the set of numbers starting at s1 and ending
   at s2 (exclusive at end) and the set starting at s2 and ending at e2 (exclusive at end)."
  [[s1 e1] [s2 e2]]
  ((comp not empty?) (intersection (set (range s1 e1)) (set (range s2 e2)))))

(defn colliding?
  "Returns true if there is a collision between a sprite and the bounding box defined by
   left and right y values (l and r) and bottom and top x values (b and t)"
  [sprite [l r t b]]
  (let [[sl sr st sb] (bounding-box sprite)]
    (and (intersect? [sl sr] [l r]) (intersect? [st sb] [t b]))))

(defn on-stairs?
  [stairs]
  (fn [world sprite]
    (let [slope (/ (- (:b stairs) (:t stairs)) (- (:r stairs) (:l stairs)))
          [l r t b] (bounding-box sprite)]
      (colliding? sprite [120 130 890 893]))))

(defn stairs
  [stairs {:l 100, :r 250, :t 575, :b 893}
   stairs (merge stairs {:on? (on-stairs? stairs)})]

   :move {:up (fn [sprite]
                (-> sprite
                    (update-in [:y] #(+ % 25))
                    (update-in [:x] #(- % 25))))
          :down (fn [sprite]
                  )}  )

(defn world
  [width height]
  (let [bg-image (image "kais_mario_cart/world-before-door.png")
        jack-left-image (image "kais_mario_cart/jack-left.png")
        jack-right-image (image "kais_mario_cart/jack-right.png")]
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
  (reset! world (assoc-in @world [:sprites id] sprite))
  (.repaint panel))

(defn sprite-y-limits [sprite world]
  (let [world-width (-> @world :background :image .getWidth)
        sprite-width (-> sprite :image .getWidth)]
    [0 (- world-width sprite-width)]))

(defn direction->fn
  [direction]
  (cond
   (#{:right :down} direction) +
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
       (= :up direction) (if (on-stairs? cur-jack)
                           ((-> @world :terrain :stairs :move :up) cur-jack)
                           cur-jack)
       (= :down direction) cur-jack
       :else (-> (update-in cur-jack [:y] #((direction->fn direction) % distance))
                 (assoc-in [:orientation] direction)
                 (assoc-in [:image] (jack-image-for direction world)))))))

(defn move-jack!
  [panel world direction]
  (let [jack (get-in @world [:sprites :jack])]
    (->>
     (update-sprite world jack (move-jack-fn direction world))
     (debug-sprite world)
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
  [& args]
  (let [world (world)
        p (panel :on-paint (draw! world) :on-key (on-key world))
        t (Timer. 15 p)
        args (apply hash-map args)]
    (doseq [k (debug-fn-keys args)]
      (activate-debug-fn! (keyword k)))
    (show-panel! (frame) p)))
