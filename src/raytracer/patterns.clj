(ns raytracer.patterns
  (:require [clojure.test :as t]
            [clojure.core.matrix :as m]
            [raytracer.core :as r]
            [raytracer.canvas :as rc]
            [raytracer.matrix :as rm]))

(def white (rc/color 1 1 1))
(def black (rc/color 0 0 0))

(defn base-pattern [type]
  {:pattern/type type
   :pattern/transform (m/identity-matrix 4)
   :pattern/transform-inv (m/identity-matrix 4)})

(defn stripe-pattern [a b]
  (merge (base-pattern :stripes)
         {:stripe/a a
          :stripe/b b}))

(defn gradient-pattern [a b]
  (merge (base-pattern :gradient)
         {:gradient/a a
          :gradient/b b}))

(defn set-pattern-transform [p t]
  (assoc p :pattern/transform t
         :pattern/transform-inv (m/inverse t)))


(defn stripe-at [s p]
  (if (zero? (mod (m/floor (r/x p)) 2))
    (:stripe/a s)
    (:stripe/b s)))

(defn gradient-at [g p]
  (let [distance (m/sub (:gradient/b g)
                        (:gradient/a g))
        x (r/x p)
        fraction (m/sub x (m/floor x))]
    (m/add (:gradient/a g) (m/mul distance fraction))))

(defmulti pattern-at (fn [s p] (:pattern/type s)))

(defmethod pattern-at :stripes [s p] (stripe-at s p))
(defmethod pattern-at :gradient [s p] (gradient-at s p))

(t/deftest stripe-test
  (t/testing "alternates in x"
    (let [p (stripe-pattern white black)]
      (t/is (= (stripe-at p (r/point 0 0 0)) white))
      (t/is (= (stripe-at p (r/point 0.9 0 0)) white))
      (t/is (= (stripe-at p (r/point 1 0 0)) black))
      (t/is (= (stripe-at p (r/point 1.1 0 0)) black))
      (t/is (= (stripe-at p (r/point -0.1 0 0)) black))
      (t/is (= (stripe-at p (r/point -1.0 0 0)) black))
      (t/is (= (stripe-at p (r/point -1.1 0 0)) white))))
  (t/testing "constant in y"
    (let [p (stripe-pattern white black)]
      (t/is (= (stripe-at p (r/point 0 0 0)) white))
      (t/is (= (stripe-at p (r/point 0 1 0)) white))
      (t/is (= (stripe-at p (r/point 0 2 0)) white))))
  (t/testing "constant in z"
    (let [p (stripe-pattern white black)]
      (t/is (= (stripe-at p (r/point 0 0 0)) white))
      (t/is (= (stripe-at p (r/point 0 0 1)) white))
      (t/is (= (stripe-at p (r/point 0 0 2)) white)))))

(t/deftest gradient-test
  (let [p (gradient-pattern white black)]
    (t/is (r/=? (pattern-at p (r/point 0 0 0)) white))
    (t/is (r/=? (pattern-at p (r/point 0.25 0 0)) (rc/color 0.75 0.75 0.75)))
    (t/is (r/=? (pattern-at p (r/point 0.5 0 0)) (rc/color 0.5 0.5 0.5)))
    (t/is (r/=? (pattern-at p (r/point 0.75 0 0)) (rc/color 0.25 0.25 0.25)))
    ))