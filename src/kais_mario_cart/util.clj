(ns kais-mario-cart.util)

(defn warn
  "Prints msg to standard error"
  [msg]
  (binding [*out* *err*]
    (println msg)))
