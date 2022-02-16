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

(defn material
  ([] (material 0.1 0.9 0.9 200.0 (rc/color 1 1 1)))
  ([ambient diffuse specular shininess color]
   {:material/ambient ambient
    :material/diffuse diffuse
    :material/specular specular
    :material/shininess shininess
    :material/color color}))

(defonce sphere-ids (atom 0))

(defn sphere
  ([] (sphere (swap! sphere-ids inc)))
  ([id] {:shape/id id
         :shape/type :sphere
         :shape/material (material)
         :shape/transform (m/identity-matrix 4)}))


(defn point-light [position intensity]
  {:light/position position
   :light/intensity intensity})

(defn set-material [s m]
  (assoc s :shape/material m))

(defn set-transform [s t]
  (assoc s :shape/transform t))

(defn intersection [t object]
  {:intersection/t t
   :intersection/object object})

(defn intersections [& is]
  (vec is))

(defn ray-transform [r m]
  (let [{:ray/keys [origin direction]} r]
    (ray (m/mmul m origin)
         (m/mmul m direction))))


(defmulti local-intersect (fn [ray shape]
                            (:shape/type shape)))
(defmethod local-intersect :sphere [ray shape]
  (let [{:ray/keys [direction origin]} ray
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
        [(intersection t1 shape)
         (intersection t2 shape)]))))

(defn intersect [ray shape]
  (let [ray2 (ray-transform ray (m/inverse (:shape/transform shape)))]
    (local-intersect ray2 shape)))

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
        canvas-pixels 400
        cnv (rc/canvas canvas-pixels canvas-pixels)
        pixel-size (/ wall-size canvas-pixels)
        half (/ wall-size 2)
        -half (m/sub 0 half)
        color (rc/color 1 0 0)
        shape (-> (sphere)
                  (set-transform (rm/scaling 1 0.5 1)))]
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

(defmulti local-normal-at (fn [s local-point]
                            (:shape/type s)) )
(defmethod local-normal-at :sphere [s local-point]
  (m/sub local-point (r/point 0 0 0)))


(defn normal-at [s world-point]
  (let [{:shape/keys [transform]} s
        object-point (m/mmul (m/inverse transform) world-point)
        object-normal (local-normal-at s object-point)
        world-normal (m/mmul (-> transform m/inverse m/transpose) object-normal)]
    (-> world-normal
        (m/mset 3 0)
        (m/normalise)))

  )


(t/deftest normals-test
  (let [s (sphere)
        s33 (m/div (m/sqrt 3) 3)]
    (t/is (= (r/vector 1 0 0)
             (normal-at s (r/point 1 0 0))))
    (t/is (= (r/vector 0 1 0)
             (normal-at s (r/point 0 1 0))))
    (t/is (= (r/vector 0 0 1)
             (normal-at s (r/point 0 0 1))))
    (t/is (= (r/vector s33 s33 s33)
             (normal-at s (r/point s33 s33 s33))))))


(t/deftest normals-transformed-test
  (let [s (-> (sphere)
              (set-transform (rm/translation 0 1 0)))
        n (normal-at s (r/point 0 1.70711 -0.70711))]
    (t/is (m/equals n (r/vector 0 0.70711 -0.70711) 0.00001)))
  (let [s (-> (sphere)
              (set-transform (m/mmul
                              (rm/scaling 1 0.5 1)
                              (rm/rotation-z (/ Math/PI 5)))))
        s22 (m/div (m/sqrt 2) 2)
        n (normal-at s (r/point 0 s22 (- s22)))]
    (t/is (m/equals n (r/vector 0 0.97014 -0.24254) 0.00001))))


(defn reflect [in normal]
  (m/sub in (m/mul normal 2 (m/dot in normal)))
  )


(t/deftest reflections-test
  (let [v (r/vector 1 -1 0)
        n (r/vector 0 1 0)
        r (reflect v n)]
    (t/is (= r (r/vector 1 1 0))))
  (let [s22 (m/div (m/sqrt 2) 2)
        v (r/vector 0 -1 0)
        n (r/vector s22 s22 0)
        r (reflect v n)]
    (t/is (m/equals r (r/vector 1 0 0)
                    0.00001))))

(defn lighting [material point light-source eyev normalv in-shadow]
  (let [{:material/keys [ambient diffuse shininess specular color]} material
        {:light/keys [intensity position]} light-source
        effective-color (m/mul color intensity)
        lightv (m/normalise (m/sub position point))
        l-ambient (m/mul effective-color ambient)
        light-dot-normal (m/dot lightv normalv)]
    (if (or (< light-dot-normal 0) in-shadow)
      l-ambient
      (let [l-diffuse (m/mul effective-color diffuse light-dot-normal)
            reflectv (reflect (r/- lightv) normalv)
            reflect-dot-eye (m/dot reflectv eyev)]
        (if (<= reflect-dot-eye 0)
          (m/add l-ambient l-diffuse)
          (let [factor (m/pow reflect-dot-eye shininess)
                l-specular (m/mul intensity specular factor)]
            (m/add l-ambient l-diffuse l-specular)))))))


(defn =? [x y]
  (m/equals x y 0.00001))

(t/deftest lighting-test
  (let [m (material)
        p (r/point 0 0 0)
        s22 (m/div (m/sqrt 2) 2)]
    (t/testing "eye between light and surface"
      (let [eyev (r/vector 0 0 -1)
            normalv (r/vector 0 0 -1)
            light (point-light (r/point 0 0 -10) (rc/color 1 1 1))]
        (t/is (=? (lighting m p light eyev normalv false)
                  (rc/color 1.9 1.9 1.9)))))
    (t/testing "eye between light and surface, in shadow"
      (let [eyev (r/vector 0 0 -1)
            normalv (r/vector 0 0 -1)
            light (point-light (r/point 0 0 -10) (rc/color 1 1 1))]
        (t/is (=? (lighting m p light eyev normalv true)
                  (rc/color 0.1 0.1 0.1)))))
    (t/testing "eye offset 45deg, between light and surface"
      (let [eyev (r/vector 0 s22 (- s22))
            normalv (r/vector 0 0 -1)
            light (point-light (r/point 0 0 -10) (rc/color 1 1 1))]
        (t/is (=? (lighting m p light eyev normalv false)
                  (rc/color 1.0 1.0 1.0)))))
    (t/testing "light offset 45deg"
      (let [eyev (r/vector 0 0 -1)
            normalv (r/vector 0 0 -1)
            light (point-light (r/point 0 10 -10) (rc/color 1 1 1))]
        (t/is (=? (lighting m p light eyev normalv false)
                  (rc/color 0.7364 0.7364 0.7364)))))
    (t/testing "eye in light reflection"
      (let [eyev (r/vector 0 (- s22) (- s22))
            normalv (r/vector 0 0 -1)
            light (point-light (r/point 0 10 -10) (rc/color 1 1 1))]
        (t/is (=? (lighting m p light eyev normalv false)
                  (rc/color 1.6364 1.6364 1.6364)))))
    (t/testing "light behind surface"
      (let [eyev (r/vector 0 0 -1)
            normalv (r/vector 0 0 -1)
            light (point-light (r/point 0 0 10) (rc/color 1 1 1))]
        (t/is (=? (lighting m p light eyev normalv false)
                  (rc/color 0.1 0.1 0.1)))))))


(defn draw-sphere-3d []
  (let [ray-origin (r/point 0 0 -5)
        wall-z 10.0
        wall-size 7.0
        canvas-pixels 1500
        cnv (rc/canvas canvas-pixels canvas-pixels)
        pixel-size (/ wall-size canvas-pixels)
        half (/ wall-size 2)
        -half (m/sub 0 half)
        light (point-light (r/point -10 10 -10)
                           (rc/color 1 1 1))
        shape (-> (sphere)
                  (set-material (assoc (material) :material/color (rc/color 1 0.2 1)))
                  (set-transform (rm/scaling 1 1 1)))]
    (doseq [y (range canvas-pixels)
            :let [world-y (m/sub half (m/mul pixel-size y))]]
      (doseq [x (range canvas-pixels)]
        (let [world-x (m/add -half (m/mul pixel-size x))
              world-position (r/point world-x world-y wall-z)
              r (ray ray-origin (r/normalise (r/- world-position ray-origin)))
              xs (intersect r shape)]
          (when-let [ray-hit (hit xs)]
            (let [point (position r (:intersection/t ray-hit))
                  normal (normal-at (:intersection/object ray-hit) point)
                  eye (r/- (:ray/direction r))
                  material (-> ray-hit :intersection/object :shape/material)
                  lighted-color (lighting material point light eye normal false)]
              (rc/pixel-write! cnv x y lighted-color))))))
    cnv))


(comment

  (time
   (-> (draw-sphere-3d)
       (rc/canvas-str)
       (->> (spit "sphere-3d.ppm"))))
  )