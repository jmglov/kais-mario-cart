(ns kais-mario-cart.core-test
  (:require [clojure.test :refer :all]
            [kais-mario-cart.core :as kmc]))

(defn reset-levels [f]
  (kmc/reset-levels!)
  (f))

(use-fixtures :each reset-levels)

(def img-name "kais_mario_cart/jack-left.png")

(deftest image->bounding-box
  (let [img (kmc/image img-name)]
    (testing "x and y required"
      (is (thrown? AssertionError (kmc/image->bounding-box img))))
    (testing ""
      (is (= {:x 1095, :y 627, :w 168, :h 266, :on-collision []}
             (kmc/image->bounding-box img :x 1095 :y 627))))))

(deftest deflevel
  (testing ":image required"
    (is (thrown-with-msg?
         AssertionError #":image argument required"
         (kmc/deflevel 1 :on-victory []))))

  (testing ":on-victory required"
    (is (thrown-with-msg?
         AssertionError #":on-victory argument required"
         (kmc/deflevel 1 :image img-name))))

  (testing "level defined"
    (is (= 1 (kmc/deflevel 1 :image img-name, :on-victory [])))))

(deftest defelement
  (testing "devlevel required first"
    (is (thrown-with-msg?
         IllegalStateException #"deflevel required before defelement"
         (kmc/defelement foo :image img-name :x 1, :y 1))))

  (let [_ (kmc/deflevel 1 :image img-name, :on-victory [])]
    (testing ":image required"
      (is (thrown-with-msg?
           AssertionError #":image argument required"
           (kmc/defelement foo :x 1, :y 1))))

    (testing ":x required"
      (is (thrown-with-msg?
           AssertionError #":x argument required"
           (kmc/defelement foo :image img-name, :y 1))))

    (testing ":y required"
      (is (thrown-with-msg?
           AssertionError #":y argument required"
           (kmc/defelement foo :image img-name, :x 1))))

    (testing "element defined"
      (is (= [1 :elements :foo]
             @(kmc/defelement foo :image img-name, :x 1, :y 1))))))

(deftest defaction
  (testing "action defined"
    (is (fn? @(kmc/defaction foo "whatever")))))

(deftest defcontrol
  (testing "devlevel required first"
    (is (thrown-with-msg?
         IllegalStateException #"deflevel required before defcontrol"
         (kmc/defcontrol foo 1 "whatever"))))

  (testing "control defined"
    (let [a (atom 1)
          b (atom {})]
      (kmc/deflevel 1 :image img-name, :on-victory [])
      (kmc/defcontrol do-stuff 27
        (swap! a inc)
        (swap! b assoc-in [:a] @a))
      (let [control (get-in @kmc/levels [1 :controls 27])]
        (is (fn? control))
        (control)
        (is (= 2 @a))
        (is (= {:a 2} @b))))))

(deftest move
  (kmc/deflevel 1 :image img-name :on-victory [])

  (testing "can't move element with more than one bounding box"
    (kmc/defelement boxes :image img-name, :x 1, :y 1, :bounding-boxes [{} {}])
    (is (thrown-with-msg?
         IllegalArgumentException #"cannot move element with multiple bounding boxes"
         (kmc/move boxes))))

  (defn xy [element]
    (-> (get-in @kmc/levels element)
        :bounding-boxes
        first
        (map [:x :y])))

  (testing "x and y components of move default to 0"
    (kmc/defelement unmoved :image img-name, :x 1, :y 1)
    (kmc/move unmoved)
    (is (= [1 1] (xy unmoved))))

  (testing "element is moved"
    (kmc/defelement moved :image img-name, :x 1, :y 1)
    (kmc/move moved :x 5 :y -1)
    (is (= [6 0] (xy moved)))))
