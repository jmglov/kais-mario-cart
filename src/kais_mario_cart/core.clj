(ns kais-mario-cart.core
  (:require [clojure.java.io :as io]
            [clojure.set :refer [intersection]]
            [clojure.string :refer [join split]]
            [kais-mario-cart.util :refer [warn]])
  (:import (java.awt Color Dimension)
           (javax.swing JFrame JOptionPane JPanel Timer)
           (java.awt.event ActionListener KeyEvent KeyListener)))

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

(def keycode->direction {KeyEvent/VK_LEFT :left
                         KeyEvent/VK_RIGHT :right
                         KeyEvent/VK_UP :up
                         KeyEvent/VK_DOWN :down})

(def keycode->action (merge
                      keycode->direction
                      {KeyEvent/VK_ESCAPE :quit}))

(defn is-action?
  [action keycode]
  (= action (keycode->action keycode)))

(defn image->bounding-box
  "Returns a bounding box for an image at the specified x and y coordinate.
  A list of actions to take on collision may be speciied as :on-collision."
  [img & {:keys [x y] :as args}]
  {:pre [(and x y)]}

  {:x x, :y y, :w (.getWidth img), :h (.getHeight img)
   :on-collision (or (:on-collision args) [])})

(def levels (atom {}))
(def current-level (atom nil))

(defn get-level []
  (@levels @current-level))

(defn reset-levels! []
  (reset! levels {})
  (reset! current-level nil))

(defmacro deflevel [index & {:keys [image on-victory]}]
  `(do
     (doseq [[~'k ~'v] [[:image ~image] [:on-victory ~on-victory]]]
       (assert ~'v (str ~'k " argument required")))
     (swap! levels assoc ~index {:image (image ~image)
                                 :is-victory? false
                                 :on-victory ~on-victory})
     (reset! current-level ~index)))

(defmacro defelement [element-name & {:keys [image x y z speed bounding-boxes]}]
  `(if @current-level
     (do
       (doseq [[~'k ~'v] [[:image ~image] [:x ~x] [:y ~y]]]
         (assert ~'v (str ~'k " argument required")))

       (let [~'element-key (keyword '~element-name)
             ~'path [@current-level :elements ~'element-key]
             ~'image (image ~image)]
         (swap! levels assoc-in ~'path {:image ~'image
                                        :z (or ~z 0)
                                        :speed (or ~speed 0)
                                        :bounding-boxes (or ~bounding-boxes
                                                            [(image->bounding-box ~'image :x ~x, :y ~y)])})
         (def ~element-name ~'path)))
     (throw (IllegalStateException. "deflevel required before defelement"))))

(defmacro defaction [action-name & body]
  `(def ~action-name (fn [& ~'args] ~@body)))

(defmacro defcontrol [control-name keycode & body]
  `(if @current-level
     (swap! levels assoc-in
            [@current-level :controls ~keycode]
            (fn [& ~'args] ~@body))
     (throw (IllegalStateException. "deflevel required before defcontrol"))))

(defn xy [obj]
  (let [lookup (fn [dim] (or (obj dim) (-> obj :bounding-boxes first dim) 0))]
    (map lookup [:x :y])))

(defn repaint [graphics widget]
  (let [level (get-level)]
    (draw-image! graphics widget (:image level) 0 0)
    (doseq [[_ element] (:elements level)]
      (apply draw-image! (concat [graphics widget] [(:image element)] (xy element))))))

(defn operate [keycode widget]
  (doseq [[_ control] (filter #(= keycode (key %)) (:controls (get-level)))]
    (control))
  (repaint (.getGraphics widget) widget))

(defn make-frame
  [title]
  (JFrame. title))

(defn make-panel
  [& {:keys [on-action width height]
      :or {width 1263, height 893}}]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (repaint g this))

    (actionPerformed [e]
      (when on-action
        (on-action e this))
      (.repaint this))

    (keyPressed [e]
      (operate (.getKeyCode e) this))

    (getPreferredSize []
      (Dimension. width height))

    (keyReleased [e])
    (keyTyped [e])))

(defn move-element [element x y]
  (let [bounding-boxes (:bounding-boxes element)]
    (if (not= 1 (count bounding-boxes))
      (throw (IllegalArgumentException. "cannot move element with multiple bounding boxes"))
      (let [bounding-box (first bounding-boxes)
            new-x (+ (:x bounding-box) x)
            new-y (+ (:y bounding-box) y)]
        (assoc element :bounding-boxes [(merge bounding-box {:x new-x, :y new-y})])))))

(defn move [element-path & {:keys [x y]}]
  (let [x (or x 0)
        y (or y 0)]
    (swap! levels update-in element-path move-element x y)))
