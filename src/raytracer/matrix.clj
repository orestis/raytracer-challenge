(ns raytracer.matrix
  (:require
   [clojure.test :as t]
   [raytracer.core :as c]
   [raytracer.canvas :as rc]
   [clojure.core.matrix :as m]))

(t/deftest matrix-basic
  (t/is (= 1.0
           (m/mget (m/matrix [[1 2] [3 4]])
                   0 0)))
  (t/is
   (= (m/matrix [[1 2] [3 4]])
      (m/matrix [[1 2] [3 4]])))
  (t/is
   (not= (m/matrix [[1.5 2] [3 4]])
         (m/matrix [[1 2] [3 4]]))))


(t/deftest matrix-mul
  (t/is (=
         (m/matrix [[20 22 50 48]
                    [44 54 114 108]
                    [40 58 110 102]
                    [16 26 46 42]])
         (m/mmul (m/matrix [[1 2 3 4]
                            [5 6 7 8]
                            [9 8 7 6]
                            [5 4 3 2]])
                 (m/matrix [[-2 1 2 3]
                            [3 2 1 -1]
                            [4 3 6 5]
                            [1 2 7 8]]))))


  (t/is (= (m/array [18 24 33 1])
           (m/mmul (m/matrix [[1 2 3 4]
                              [2 4 4 2]
                              [8 6 4 1]
                              [0 0 0 1]])
                   (m/array [1 2 3 1])))))



(t/deftest matrix-inversion
  (t/is (= -2120.0
           (m/det (m/matrix [[6 4 4 4]
                             [5 5 7 6]
                             [4 -9 3 -7]
                             [9 1 7 -6]]))))
  (t/is (= 0.0
           (m/det (m/matrix [[-4 2 -2 -3]
                             [9 6 2 6]
                             [0 -5 1 -5]
                             [0 0 0 0]]))))

  (t/is (m/equals (m/matrix [[0.21805 0.45113 0.24060 -0.04511]
                             [-0.80827 -1.45677 -0.44361 0.52068]
                             [-0.07895 -0.22368 -0.05263 0.19737]
                             [-0.52256 -0.81391 -0.30075 0.30639]])
                  (m/inverse (m/matrix [[-5 2 6 -8]
                                        [1 -5 1 8]
                                        [7 7 -6 -7]
                                        [1 -3 7 4]]))
                  0.00001)))

(defn translation [x y z]
  (m/matrix [[1 0 0 x]
             [0 1 0 y]
             [0 0 1 z]
             [0 0 0 1]]))

(defn scaling [x y z]
  (m/matrix [[x 0 0 0]
             [0 y 0 0]
             [0 0 z 0]
             [0 0 0 1]]))


(t/deftest matrix-translation
  (t/is (= (c/point 2 1 7)
           (m/mmul (translation 5 -3 2)
                   (c/point -3 4 5))))
  (t/is (= (c/point -8 7 3)
           (m/mmul (m/inverse (translation 5 -3 2))
                   (c/point -3 4 5))))
  (t/is (= (c/vector -3 4 5)
           (m/mmul (translation 5 -3 2)
                   (c/vector -3 4 5)))))


(t/deftest matrix-scaling
  (t/is (= (c/point -8 18 32)
           (m/mmul (scaling 2 3 4)
                   (c/point -4 6 8))))
  (t/is (= (c/vector -2 2 2)
           (m/mmul (m/inverse (scaling 2 3 4))
                   (c/vector -4 6 8))))
  (t/is (= (c/vector -8 18 32)
           (m/mmul (scaling 2 3 4)
                   (c/vector -4 6 8)))))

(defn rotation-x [r]
  (m/matrix [[1 0 0 0]
             [0 (m/cos r) (- (m/sin r)) 0]
             [0 (m/sin r) (m/cos r) 0]
             [0 0 0 1]]))

(defn rotation-y [r]
  (m/matrix [[(m/cos r) 0 (m/sin r) 0]
             [0 1 0 0]
             [(- (m/sin r)) 0  (m/cos r) 0]
             [0 0 0 1]]))

(defn rotation-z [r]
  (m/matrix [[(m/cos r) (- (m/sin r)) 0 0]
             [(m/sin r) (m/cos r)  0 0]
             [0 0 1 0]
             [0 0 0 1]]))


(t/deftest matrix-rotation
  (let [p (c/point 0 1 0)
        s22 (/ (m/sqrt 2) 2)
        half-quarter (rotation-x (/ Math/PI 4))
        full-quarter (rotation-x (/ Math/PI 2))]
    (t/is (m/equals (c/point 0 s22 s22)
                    (m/mmul half-quarter p)
                    0.00001))
    (t/is (m/equals (c/point 0 0 1)
                    (m/mmul full-quarter p)
                    0.00001)))
  (let [p (c/point 0 0 1)
        s22 (/ (m/sqrt 2) 2)
        half-quarter (rotation-y (/ Math/PI 4))
        full-quarter (rotation-y (/ Math/PI 2))]
    (t/is (m/equals (c/point s22 0 s22)
                    (m/mmul half-quarter p)
                    0.00001))
    (t/is (m/equals (c/point 1 0 0)
                    (m/mmul full-quarter p)
                    0.00001)))
  (let [p (c/point 0 1 0)
        s22 (/ (m/sqrt 2) 2)
        half-quarter (rotation-z (/ Math/PI 4))
        full-quarter (rotation-z (/ Math/PI 2))]
    (t/is (m/equals (c/point (- s22) s22 0)
                    (m/mmul half-quarter p)
                    0.00001))
    (t/is (m/equals (c/point -1 0 0)
                    (m/mmul full-quarter p)
                    0.00001))))


(defn shearing [xy xz yx yz zx zy]
  (m/matrix [[1 xy xz 0]
             [yx 1 yz 0]
             [zx zy 1 0]
             [0 0 0 1]]))

(t/deftest matrix-shearing
  (let [p (c/point 2 3 4)]
    (t/is (= (c/point 5 3 4)
             (m/mmul (shearing 1 0 0 0 0 0) p)))
    (t/is (= (c/point 6 3 4)
             (m/mmul (shearing 0 1 0 0 0 0) p)))
    (t/is (= (c/point 2 5 4)
             (m/mmul (shearing 0 0 1 0 0 0) p)))
    (t/is (= (c/point 2 7 4)
             (m/mmul (shearing 0 0 0 1 0 0) p)))
    (t/is (= (c/point 2 3 6)
             (m/mmul (shearing 0 0 0 0 1 0) p)))
    (t/is (= (c/point 2 3 7)
             (m/mmul (shearing 0 0 0 0 0 1) p)))))


(t/deftest chained-transforms
  (let [e 0.000001
        p (c/point 1 0 1)
        A (rotation-x (/ Math/PI 2))
        B (scaling 5 5 5)
        C (translation 10 5 7)
        T (m/mmul C B A)
        p2 (m/mmul A p)
        p3 (m/mmul B p2)
        p4 (m/mmul C p3)]
    (t/is (m/equals (c/point 1 -1 0) p2 e))
    (t/is (m/equals (c/point 5 -5 0) p3 e))
    (t/is (m/equals (c/point 15 0 7) p4 e))
    (t/is (m/equals (c/point 15 0 7)
                    (m/mmul T p) e))))


(defn draw-clock []
  (let [cnv (rc/canvas 128 128)
        color (rc/color 1 1 1)
        twelve (c/point 0 0 1)
        ->canvas-coords (fn [x]
                          (int (+ 64 (* 40 x))))]
    (doseq [hour (range 12)]
      (let [p (m/mmul (rotation-y (* hour (/ Math/PI 6)))
                      twelve)
            x (->canvas-coords (c/x p))
            y (->canvas-coords (c/z p))]
        (rc/pixel-write! cnv x y color)))
    cnv))

#_
(-> (draw-clock)
    (rc/canvas-str)
    (->> (spit "clock.ppm")))