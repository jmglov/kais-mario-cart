(ns kais-mario-cart.util)

(defn warn
  "Prints msg to standard error"
  [msg]
  (binding [*out* *err*]
    (println msg)))

(defn not-nil?
  "Returns true if value v is not nil"
  [v]
  ((comp not nil?) v))
