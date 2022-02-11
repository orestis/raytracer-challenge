(ns raytracer.rays
  (:require [clojure.core.matrix :as m]
            [raytracer.core :as r]
            [raytracer.matrix :as rm]
            [raytracer.canvas :as rc]
            [clojure.test :as t]))

(defn ray [origin direction]
  {:ray/origin origin
   :ray/direction direction})

(defn position [ray t]
  (m/add (:ray/origin ray)
         (m/mul (:ray/direction ray) t)))

(t/deftest ray-position
  (let [ray (ray (r/point 2 3 4) (r/vector 1 0 0))]
    (t/is (= (position ray 0) (r/point 2 3 4)))
    (t/is (= (position ray 1) (r/point 3 3 4)))
    (t/is (= (position ray -1) (r/point 1 3 4)))
    (t/is (= (position ray 2.5) (r/point 4.5 3 4)))))

(defonce sphere-ids (atom 0))

(defn sphere
  ([] (sphere (swap! sphere-ids inc)))
  ([id] {:sphere/id id
         :sphere/transform (m/identity-matrix 4)}))

(defn set-transform [s t]
  (assoc s :sphere/transform t))

(defn intersection [t object]
  {:intersection/t t
   :intersection/object object})

(defn intersections [& is]
  (vec is))

(defn ray-transform [r m]
  (let [{:ray/keys [origin direction]} r]
    (ray (m/mmul m origin)
         (m/mmul m direction))))


(defn intersect [ray sphere]
  (let [ray2 (ray-transform ray (m/inverse (:sphere/transform sphere)))
        {:ray/keys [direction origin]} ray2
        sphere-to-ray (r/- origin (r/point 0 0 0))
        a (m/dot direction direction)
        b- (m/mul -2 (m/dot direction sphere-to-ray))
        c (m/sub (m/dot sphere-to-ray sphere-to-ray) 1)
        discriminant (m/sub (m/square b-)
                            (m/mul 4 a c))]
    (if (< discriminant 0)
      []
      (let [t1 (m/div (m/sub b- (m/sqrt discriminant))
                      (m/mul 2 a))
            t2 (m/div (m/add b- (m/sqrt discriminant))
                      (m/mul 2 a))]
        [(intersection t1 sphere)
         (intersection t2 sphere)]))))

(t/deftest ray-intersect
  (t/testing "two points"
    (let [r (ray (r/point 0 0 -5) (r/vector 0 0 1))
          s (sphere)
          is (intersect r s)
          xs (map :intersection/t is)]
      (t/is (m/equals [4.0 6.0] xs))))
  (t/testing "tangent"
    (let [r (ray (r/point 0 1 -5) (r/vector 0 0 1))
          s (sphere)
          is (intersect r s)
          xs (map :intersection/t is)]
      (t/is (m/equals [5.0 5.0] xs))
      (t/is (= s
               (-> is first :intersection/object)
               (-> is second :intersection/object)))))
  (t/testing "missed"
    (let [r (ray (r/point 0 2 -5) (r/vector 0 0 1))
          s (sphere)
          is (intersect r s)
          xs (map :intersection/t is)]
      (t/is (m/equals [] xs))))
  (t/testing "inside"
    (let [r (ray (r/point 0 0 0) (r/vector 0 0 1))
          s (sphere)
          is (intersect r s)
          xs (map :intersection/t is)]
      (t/is (m/equals [-1 1] xs))))
  (t/testing "outside"
    (let [r (ray (r/point 0 0 5) (r/vector 0 0 1))
          s (sphere)
          is (intersect r s)
          xs (map :intersection/t is)]
      (t/is (m/equals [-6 -4] xs)))))

(defn hit [is]
  (->> is
       (filter #(> (:intersection/t %) 0))
       (sort-by :intersection/t)
       first))

(t/deftest ray-hit
  (t/testing "positive t"
    (let [s (sphere)
          i1 (intersection 1 s)
          i2 (intersection 2 s)
          is (intersections i2 i1)]
      (t/is (= i1 (hit is)))))
  (t/testing "some negative t"
    (let [s (sphere)
          i1 (intersection -1 s)
          i2 (intersection 1 s)
          is (intersections i2 i1)]
      (t/is (= i2 (hit is)))))
  (t/testing "all negative t"
    (let [s (sphere)
          i1 (intersection -1 s)
          i2 (intersection -2 s)
          is (intersections i2 i1)]
      (t/is (= nil (hit is)))))
  (t/testing "lowest non-negative"
    (let [s (sphere)
          i1 (intersection 5 s)
          i2 (intersection 7 s)
          i3 (intersection -3 s)
          i4 (intersection 2 s)
          is (intersections i1 i2 i3 i4)]
      (t/is (= i4 (hit is))))))



(t/deftest ray-transform-test
  (t/testing "translating"
    (let [r (ray (r/point 1 2 3) (r/vector 0 1 0))
          m (rm/translation 3 4 5)
          r2 (ray-transform r m)]
      (t/is (= r2
               (ray (r/point 4 6 8)
                    (r/vector 0 1 0))))))
  (t/testing "scaling"
    (let [r (ray (r/point 1 2 3) (r/vector 0 1 0))
          m (rm/scaling 2 3 4)
          r2 (ray-transform r m)]
      (t/is (= r2
               (ray (r/point 2 6 12)
                    (r/vector 0 3 0)))))))


(t/deftest ray-intersect-transformed
  (t/testing "scaled"
    (let [r (ray (r/point 0 0 -5) (r/vector 0 0 1))
          s (-> (sphere)
                (set-transform (rm/scaling 2 2 2)))
          is (intersect r s)
          xs (map :intersection/t is)]
      (t/is (m/equals [3.0 7.0] xs))))
  (t/testing "translated"
    (let [r (ray (r/point 0 0 -5) (r/vector 0 0 1))
          s (-> (sphere)
                (set-transform (rm/translation 5 0 0)))
          is (intersect r s)]
      (t/is (= [] is)))))



(defn draw-sphere []
  (let [ray-origin (r/point 0 0 -5)
        wall-z 10.0
        wall-size 7.0
        canvas-pixels 100
        cnv (rc/canvas canvas-pixels canvas-pixels)
        pixel-size (/ wall-size canvas-pixels)
        half (/ wall-size 2)
        -half (m/sub 0 half)
        color (rc/color 1 0 0)
        shape (sphere)]
    (doseq [y (range canvas-pixels)
            :let [world-y (m/sub half (m/mul pixel-size y))]]
      (doseq [x (range canvas-pixels)]
        (let [world-x (m/add -half (m/mul pixel-size x))
              position (r/point world-x world-y wall-z)
              r (ray ray-origin (r/normalise (r/- position ray-origin)))
              xs (intersect r shape)]
          (when (hit xs)
            (rc/pixel-write! cnv x y color)))))
    cnv))


(comment
  (-> (draw-sphere)
      (rc/canvas-str)
      (->> (spit "sphere.ppm")))
  )