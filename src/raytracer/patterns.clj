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

(defn ring-pattern [a b]
  (merge (base-pattern :ring)
         {:ring/a a
          :ring/b b}))

(defn checkers-pattern [a b]
  (merge (base-pattern :checkers)
         {:checkers/a a
          :checkers/b b}))

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

(defn ring-at [r p]
  (if (zero? (mod (m/floor (m/sqrt
                            (m/add
                             (m/square (r/x p))
                             (m/square (r/z p))))) 2))
    (:ring/a r)
    (:ring/b r)))

(defn checkers-at [c p]
  (let [f (m/floor p)]
    (if (zero? (mod (m/add (r/x f) (r/y f) (r/z f)) 2))
      (:checkers/a c)
      (:checkers/b c))))

(defmulti pattern-at (fn [pattern p] (:pattern/type pattern)))

(defmethod pattern-at :stripes [s p] (stripe-at s p))
(defmethod pattern-at :gradient [s p] (gradient-at s p))
(defmethod pattern-at :ring [s p] (ring-at s p))
(defmethod pattern-at :checkers [s p] (checkers-at s p))

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

(t/deftest ring-test
  (let [p (ring-pattern white black)]
    (t/is (r/=? (pattern-at p (r/point 0 0 0)) white))
    (t/is (r/=? (pattern-at p (r/point 1 0 0)) black))
    (t/is (r/=? (pattern-at p (r/point 0 0 1)) black))
    (t/is (r/=? (pattern-at p (r/point 0.708 0 0.708)) black))))

(t/deftest checkers-test
  (t/testing "alternates in x"
    (let [p (checkers-pattern white black)]
      (t/is (= (pattern-at p (r/point 0 0 0)) white))
      (t/is (= (pattern-at p (r/point 0.99 0 0)) white))
      (t/is (= (pattern-at p (r/point 1.1 0 0)) black))))
  (t/testing "alternates in y"
    (let [p (checkers-pattern white black)]
      (t/is (= (pattern-at p (r/point 0 0 0)) white))
      (t/is (= (pattern-at p (r/point 0 0.99 0)) white))
      (t/is (= (pattern-at p (r/point 0 1.1 0)) black))))
  (t/testing "alternates in z"
    (let [p (checkers-pattern white black)]
      (t/is (= (pattern-at p (r/point 0 0 0)) white))
      (t/is (= (pattern-at p (r/point 0 0 0.99)) white))
      (t/is (= (pattern-at p (r/point 0 0 1.1)) black)))))