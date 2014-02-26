(ns kais-mario-cart.core
  (:require [seesaw.core :as gui]))

(defn make-frame []
  (gui/frame
   :title "Kai's Mario Cart"))

(defn show-frame [f]
  (-> f
      gui/pack!
      gui/show!
      (gui/config! :size [1263 :by 892])))

(defn run []
  (gui/invoke-later
   (-> (make-frame)
       show-frame
       (gui/config! :on-close :exit))))
