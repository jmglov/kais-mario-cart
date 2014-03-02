(ns kais-mario-cart.debug-test
  (:require [clojure.test :refer :all]
            [kais-mario-cart.debug :refer :all]))

(deftest test-activate-debug-fn!
  (testing "with valid function key"
    (is (activate-debug-fn! :sprite-info)))

  (testing "with invalid function key"
    (is (not (activate-debug-fn! :not-bloody-likely)))))

(deftest test-activate-debug-fns-from-args!
  (testing "with empty args"
    (is (empty? (activate-debug-fns-from-args! []))))
  (testing "without --debug"
    (is (empty? (activate-debug-fns-from-args! ["--foo"]))))
  (testing "with valid key"
    (activate-debug-fns-from-args! ["--debug sprite-info"])
    (is (= :sprite-info (first active-fns)))))
