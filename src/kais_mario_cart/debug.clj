(ns kais-mario-cart.debug
  (:require [kais-mario-cart.util :refer [not-nil? warn]]
            [clojure.string :refer [join split]]
            [clojure.tools.cli :as cli]))

(def ^:private active (atom []))

(def ^:private available
  {:sprite-info (fn [_ sprite] (println (map sprite [:y :x :orientation])))})

(defn clear!
  "Removes all active debug functions"
  []
  (reset! active []))

(defn activate!
  "Activates the debug function corresponding to a function key. Returns true if the
   function key was valid."
  [fn-key]
  (let [f (or (available fn-key) (available (keyword fn-key)))
        valid? (not-nil? f)]
    (if f
      (swap! active conj [fn-key f])
      (warn (str "Debug function " fn-key " not found! Available functions are: "
                 (join " " (map name (keys available))))))
    valid?))

(def cli-options
  [["-d" "--debug DEBUG_KEY1[,DEBUG_KEY2...]"
    :default []
    :parse-fn #(split % #",")]])

(defn activate-from-args!
  "Activates the debug functions corresponding to the value of the --debug command line argument,
   if present. Returns true if all function keys were valid."
  [args]
  (->>
   (map #(activate! (keyword %)) (get-in (cli/parse-opts args cli-options) [:options :debug]))
   (every? true?)))

(defn active-fns
  "Returns a list keys corresponding to active debug functions"
  []
  (map first @active))

(defn- debug
  [world sprite retval]
  (doseq [[_ f] @active]
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
