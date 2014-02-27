(ns kais-mario-cart.core
  (:require [clojure.java.io :as io])
  (:import (java.awt Color Dimension)
           (javax.swing JFrame JOptionPane JPanel Timer)
           (java.awt.event ActionListener KeyEvent KeyListener)))

(defn frame []
  (JFrame. "Kai's Mario Cart"))

(defn panel [painters]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (doseq [painter painters] (painter g this)))
    (actionPerformed [e]
      (.repaint this))
    (keyPressed [e])
    (getPreferredSize []
      (Dimension. 1263 893))
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
