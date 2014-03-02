(ns kais-mario-cart.debug
  (:require [kais-mario-cart.util :refer [warn]]
            [clojure.string :refer [join split]]))

(def ^:private active-debug-fns (atom []))

(def debug-fns
  {:sprite-info (fn [_ sprite] (println (map sprite [:y :x :orientation])))})

(defn debug-fn-keys
  [args]
  (if (args "--debug")
    (split (or (args "--debug") "") #",")
    []))

(defn activate-debug-fn!
  [fn-key]
  (let [f (debug-fns fn-key)]
    (if f
      (swap! active-debug-fns conj f)
      (warn (str "Debug function " fn-key " not found! Available functions are: "
                 (join " " (map name (keys debug-fns))))))))

(defn activate-debug-fns-from-args!
  [args]
  (doseq [k (debug-fn-keys (apply hash-map args))]
    (activate-debug-fn! (keyword k))))

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
