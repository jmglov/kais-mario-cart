(ns kais-mario-cart.core
  (:require [clojure.java.io :as io])
  (:import (java.awt Color Dimension)
           (javax.swing JFrame JOptionPane JPanel Timer)
           (java.awt.event ActionListener KeyEvent KeyListener)))

(defn frame []
  (JFrame. "Kai's Mario Cart"))

(defn panel [& {:keys [on-paint on-action on-key width height]
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

(defn sprite
  "Read a BufferedImage from a file"
  [file]
  (javax.imageio.ImageIO/read (io/input-stream (io/resource file))))

(defn show-panel! [frame panel]
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

(defn draw! [world]
  (fn [graphics widget]
    (apply draw-image! graphics widget (map (:background @world) [:image :y :x]))
    (doseq [s (:sprites @world)]
      (apply draw-image! graphics widget (map s [:image :y :x])))))

(defn world []
  (atom {:background {:image (sprite "kais_mario_cart/world-before-door.png"), :y 0, :x 0}
         :sprites [{:image (sprite "kais_mario_cart/jack-left.png"), :y (- 1263 168), :x (- 893 266)}]}))

(defn on-key [world]
  (fn [keycode panel]
    (cond
     (= keycode KeyEvent/VK_ESCAPE) (System/exit 0)
     (= keycode KeyEvent/VK_LEFT) (do
                                    (reset! world (update-in @world [:sprites 0 :y] #(- % 25)))
                                    (.repaint panel))
     (= keycode KeyEvent/VK_RIGHT) (do
                                    (reset! world (update-in @world [:sprites 0 :y] #(+ % 25)))
                                    (.repaint panel)))))

(defn -main [& args]
  (let [world (world)
        p (panel :on-paint (draw! world) :on-key (on-key world))
        t (Timer. 15 p)]
    (show-panel! (frame) p)))
