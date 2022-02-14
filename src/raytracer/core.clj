(ns raytracer.core
  (:require [clojure.core.matrix :as m]
            [clojure.test :as t])
  (:refer-clojure :exclude [vector vector? + - * /]))

(m/set-current-implementation :vectorz)
(set! *warn-on-reflection* true)

(defn tuple [& args]
  (m/array args))

(defn x [t]
  (m/mget t 0))

(defn y [t]
  (m/mget t 1))

(defn z [t]
  (m/mget t 2))

(defn w [t]
  (m/mget t 3))

(defn point? [t]
  (= (w t) 1.0))

(defn vector? [t]
  (= (w t) 0.0))

(t/deftest tuples
  (t/testing "point"
    (let [a (tuple 4.3, -4.2, 3.1, 1.0)]
      (t/is (= (x a) 4.3))
      (t/is (= (y a) -4.2))
      (t/is (= (z a) 3.1))
      (t/is (= (w a) 1.0))
      (t/is (= true (point? a)))
     (t/is (= false (vector? a))) ))
  (t/testing "vector"
    (let [a (tuple 4.3, -4.2, 3.1, 0.0)]
      (t/is (= (x a) 4.3))
      (t/is (= (y a) -4.2))
      (t/is (= (z a) 3.1))
      (t/is (= (w a) 0.0))
      (t/is (= false (point? a)))
      (t/is (= true (vector? a))))))

(defn point [x y z]
  (tuple x y z 1.0))

(defn vector [x y z]
  (tuple x y z 0.0))

(t/deftest tuples-2
  (t/is (= (tuple 4 -4 3 1)
           (point 4 -4 3)))
  (t/is (= (tuple 4 -4 3 0)
           (vector 4 -4 3))))

(defn + [t1 t2]
  (m/add t1 t2))

(t/deftest tuples-3
  (t/is (= (tuple 1 1 6 1)
           (+ (tuple 3 -2 5 1)
                (tuple -2 3 1 0)))))

(defn -
  ([t]
   (- (tuple 0 0 0 0)
      t))
  ([t1 t2]
   (m/sub t1 t2)))

(t/deftest tuples-4
  (t/is (= (vector -2 -4 -6)
           (- (point 3 2 1)
              (point 5 6 7))))
  (t/is (= (point -2 -4 -6)
           (- (point 3 2 1)
              (vector 5 6 7))))
  (t/is (= (vector -2 -4 -6)
           (- (vector 3 2 1)
              (vector 5 6 7)))))

(t/deftest tuples-5
  (t/is (= (tuple -1 2 -3 -4)
           (- (tuple 1 -2 3 4)))))


(defn * [t s]
  (m/mul t s))

(t/deftest tuples-6
  (t/is (= (tuple 3.5 -7 10.5 -14)
           (* (tuple 1 -2 3 -4) 3.5)))
  (t/is (= (tuple 0.5 -1 1.5 -2)
           (* (tuple 1 -2 3 -4) 0.5))))

(defn / [t s]
  (m/div t s))

(t/deftest tuples-7
  (t/is (= (tuple 0.5 -1 1.5 -2)
           (/ (tuple 1 -2 3 -4) 2))))

(defn magnitude [v]
  (m/magnitude v))

(t/deftest tuples-8
  (t/is (= 1.0
           (magnitude (vector 0 1 0))
           (magnitude (vector 0 0 1))))
  (t/is (= (m/sqrt 14)
           (magnitude (vector 1 2 3))
           (magnitude (vector -1 -2 -3)))))

(defn normalise [v]
  (m/normalise v))

(t/deftest tuples-9
  (t/is (= (vector 1 0 0)
           (normalise (vector 4 0 0))))
  (t/is (= (vector (/ 1 (m/sqrt 14))
                   (/ 2 (m/sqrt 14))
                   (/ 3 (m/sqrt 14)))
           (normalise (vector 1 2 3))))
  (t/is (= 1.0
           (magnitude (normalise (vector 1 2 3))))))


(defn dot [v1 v2]
  (m/dot v1 v2))

(t/deftest tuples-10
  (t/is (= 20.0 (dot (vector 1 2 3)
                     (vector 2 3 4)))))


(m/select-view (vector 1 2 3) :butlast)

(defn cross [v1 v2]
  (let [v3 (m/cross (m/select v1 :butlast)
                    (m/select v2 :butlast))]
    (m/conjoin v3 0)))

(t/deftest tuples-11
  (let [a (vector 1 2 3)
        b (vector 2 3 4)]
    (t/is (= (vector -1 2 -1)
             (cross a b)))
    (t/is (= (vector 1 -2 1)
             (cross b a)))))