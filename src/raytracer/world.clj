(ns raytracer.world
  (:require [clojure.core.matrix :as m]
            [raytracer.core :as r]
            [raytracer.rays :refer
             [set-transform set-material
              material ray intersect
              sphere point-light]
             :as rays]
            [raytracer.matrix :as rm]
            [raytracer.canvas :as rc]
            [clojure.test :as t]))


(defn default-world []
  {:world/light (point-light (r/point -10 10 -10)
                             (rc/color 1 1 1))
   :world/objects [(-> (sphere)
                       (set-material
                        (material 0.1 0.7 0.2 200
                                  (rc/color 0.8 1.0 0.6))))
                   (-> (sphere)
                       (set-transform
                        (rm/scaling 0.5 0.5 0.5)))
                   ]})


(defn intersect-world [world ray]
  (let [objs (:world/objects world)]
    (->> objs
         (mapcat #(intersect ray %))
         (sort-by :intersection/t))))

(t/deftest world-intersect-test
  (let [w (default-world)
        r (ray (r/point 0 0 -5) (r/vector 0 0 1))
        xs (intersect-world w r)]
    (t/is (= [4.0 4.5 5.5 6.0]
             (map :intersection/t xs)))))


(def EPSILON 0.00001)
(defn prepare-computations [{:intersection/keys [t object] :as isection}
                            ray]
  (let [point (rays/position ray t)
        normalv (rays/normal-at object point)
        eyev (r/- (:ray/direction ray))
        inside (neg? (m/dot normalv eyev))
        normalv (if inside (r/- normalv) normalv)]
    (assoc isection
           :comps/inside inside
           :comps/point point
           :comps/over-point (m/add point (m/mul normalv EPSILON))
           :comps/eyev eyev
           :comps/normalv normalv)))

(t/deftest computations-test
  (let [r (ray (r/point 0 0 -5) (r/vector 0 0 1))
        s (sphere)
        i (rays/intersection 4 s)
        comps (select-keys (prepare-computations i r)
                           [:intersection/t :intersection/object
                            :comps/point :comps/eyev :comps/normalv :comps/inside])
        ]
    (t/is (= {:intersection/t (:intersection/t i)
              :intersection/object (:intersection/object i)
              :comps/point (r/point 0 0 -1)
              :comps/eyev (r/vector 0 0 -1)
              :comps/normalv (r/vector 0 0 -1)
              :comps/inside false}
             comps)))
  (let [r (ray (r/point 0 0 0) (r/vector 0 0 1))
        s (sphere)
        i (rays/intersection 1 s)
        comps (select-keys (prepare-computations i r)
                           [:intersection/t :intersection/object
                            :comps/point :comps/eyev :comps/normalv :comps/inside])]
    (t/is (= {:intersection/t (:intersection/t i)
              :intersection/object (:intersection/object i)
              :comps/point (r/point 0 0 1)
              :comps/eyev (r/vector 0 0 -1)
              :comps/normalv (r/vector 0 0 -1)
              :comps/inside true}
             comps)))
  (let [r (ray (r/point 0 0 -5) (r/vector 0 0 1))
        s (-> (sphere)
              (set-transform (rm/translation 0 0 1)))
        i (rays/intersection 5 s)
        comps (prepare-computations i r)
        {:comps/keys [over-point point]} comps]
    (t/is (< (r/z over-point) (- (/ EPSILON 2))))
    (t/is (> (r/z point) (r/z over-point)))))

(defn shadowed? [world point]
  (let [v (m/sub (-> world :world/light :light/position) point)
        distance (m/magnitude v)
        direction (m/normalise v)
        r (rays/ray point direction)
        is (intersect-world world r)
        h (rays/hit is)]
    (if (and h (< (:intersection/t h) distance))
      true
      false)))

(t/deftest shadowed-test
  (t/testing "nothing is collinear with point and light"
    (let [w (default-world)
          p (r/point 0 10 0)]
      (t/is (= false (shadowed? w p)))))
  (t/testing "object between point and light"
    (let [w (default-world)
          p (r/point 10 -10 10)]
      (t/is (= true (shadowed? w p)))))
  (t/testing "object behind light"
    (let [w (default-world)
          p (r/point -20 20 -20)]
      (t/is (= false (shadowed? w p)))))
  (t/testing "object behind point"
    (let [w (default-world)
          p (r/point -2 2 -2)]
      (t/is (= false (shadowed? w p))))))

(defn shade-hit [world comps]
  (rays/lighting
   (-> comps :intersection/object :shape/material)
   (:comps/point comps)
   (:world/light world)
   (:comps/eyev comps)
   (:comps/normalv comps)
   (shadowed? world (:comps/over-point comps))))

(t/deftest shade-hit-test
  (let [w (default-world)
        r (ray (r/point 0 0 -5) (r/vector 0 0 1))
        shape (-> w :world/objects first)
        i (rays/intersection 4 shape)
        comps (prepare-computations i r)
        c (shade-hit w comps)]
    (t/is (rays/=? c
                   (rc/color 0.38066 0.47583 0.2855))))
  (let [w (-> (default-world)
              (assoc :world/light (point-light (r/point 0 0.25 0)
                                               (rc/color 1 1 1))))
        r (ray (r/point 0 0 0) (r/vector 0 0 1))
        shape (-> w :world/objects second)
        i (rays/intersection 0.5 shape)
        comps (prepare-computations i r)
        c (shade-hit w comps)]
    (t/is (rays/=? c
                   (rc/color 0.90498 0.90498 0.90498)))))

(t/deftest shade-hit-test-shadow
  (let [w (-> (default-world)
              (assoc :world/light (point-light (r/point 0 0 -10)
                                               (rc/color 1 1 1)))
              (assoc :world/objects [(sphere)
                                     (-> (sphere)
                                         (set-transform (rm/translation 0 0 10)))]))
        r (ray (r/point 0 0 5) (r/vector 0 0 1))
        shape (-> w :world/objects second)
        i (rays/intersection 4 shape)
        comps (prepare-computations i r)
        c (shade-hit w comps)]
    (t/is (rays/=? c
                   (rc/color 0.1 0.1 0.1)))))

(defn color-at [world ray]
  (let [xs (intersect-world world ray)
        the-hit (rays/hit xs)]
    (if the-hit
      (let [comps (prepare-computations the-hit ray)]
        (shade-hit world comps))
      (rc/color 0 0 0))))

(t/deftest color-at-test
  (t/testing "miss"
    (let [w (default-world)
          r (ray (r/point 0 0 -5) (r/vector 0 1 0))
          c (color-at w r)]
      (t/is (= c (rc/color 0 0 0)))))
  (t/testing "hit"
    (let [w (default-world)
          r (ray (r/point 0 0 -5) (r/vector 0 0 1))
          c (color-at w r)]
      (t/is (rays/=? c (rc/color 0.38066, 0.47583, 0.2855)))))
  (t/testing "behind"
    (let [w (-> (default-world)
                (update-in [:world/objects 0] assoc-in [:shape/material :material/ambient] 1.0)
                (update-in [:world/objects 1] assoc-in [:shape/material :material/ambient] 1.0))
          r (ray (r/point 0 0 0.75) (r/vector 0 0 -1))
          c (color-at w r)]
      (t/is (= c (-> w :world/objects second :shape/material :material/color))))))


(defn view-transform [from to up]
  (let [forward (m/normalise (m/sub to from))
        upn (m/normalise up)
        left (r/cross forward upn)
        true-up (r/cross left forward)
        orientation (m/matrix
                     [[(r/x left) (r/y left) (r/z left) 0]
                      [(r/x true-up) (r/y true-up) (r/z true-up) 0]
                      [(- (r/x forward)) (- (r/y forward)) (- (r/z forward)) 0]
                      [0 0 0 1]])]
    (m/mmul orientation (rm/translation (- (r/x from)) (- (r/y from)) (- (r/z from))))))


(t/deftest view-transform-test
  (t/testing "default"
    (let [from (r/point 0 0 0)
          to (r/point 0 0 -1)
          up (r/vector 0 1 0)
          t (view-transform from to up)]
      (t/is (rays/=? t (m/identity-matrix 4)))))
  (t/testing "positive z"
    (let [from (r/point 0 0 0)
          to (r/point 0 0 1)
          up (r/vector 0 1 0)
          t (view-transform from to up)]
      (t/is (rays/=? t (rm/scaling -1 1 -1)))))
  (t/testing "move the world"
    (let [from (r/point 0 0 8)
          to (r/point 0 0 0)
          up (r/vector 0 1 0)
          t (view-transform from to up)]
      (t/is (rays/=? t (rm/translation 0 0 -8)))))
  (t/testing "arbitrary"
    (let [from (r/point 1 3 2)
          to (r/point 4 -2 8)
          up (r/vector 1 1 0)
          t (view-transform from to up)]
      (t/is (rays/=? t
                     (m/matrix [[-0.50709 0.50709 0.67612 -2.36643]
                                [0.76772 0.60609 0.12122 -2.82843]
                                [-0.35857 0.59761 -0.71714 0.00000]
                                [0.00000 0.00000 0.00000 1.00000]]))))))


(defn new-camera [hsize vsize fov]
  (let [half-view (m/tan (m/div fov 2.0))
        aspect (m/div hsize vsize)
        [half-width half-height] (if (>= aspect 1.0)
                                   [half-view (m/div half-view aspect)]
                                   [(m/mul half-view aspect) half-view])
        pixel-size (m/div (m/mul half-width 2.0) hsize)]
    {:camera/hsize hsize
     :camera/vsize vsize
     :camera/fov fov
     :camera/pixel-size pixel-size
     :camera/half-width half-width
     :camera/half-height half-height
     :camera/transform (m/identity-matrix 4)}))

(t/deftest camera-pixel-size-test
  (t/testing "horizontal"
    (let [c (new-camera 200 125 (/ Math/PI 2))]
      (t/is (rays/=? 0.01
               (:camera/pixel-size c)))))
  (t/testing "vertical"
    (let [c (new-camera 125 200 (/ Math/PI 2))]
      (t/is (rays/=? 0.01
               (:camera/pixel-size c))))))


(defn ray-for-pixel [{:camera/keys [pixel-size half-height half-width transform]} px py]
  (let [xoffset (m/mul (+ px 0.5) pixel-size)
        yoffset (m/mul (+ py 0.5) pixel-size)
        world-x (- half-width xoffset)
        world-y (- half-height yoffset)
        pixel (m/mmul (m/inverse transform) (r/point world-x world-y -1))
        origin (m/mmul (m/inverse transform) (r/point 0 0 0))
        direction (m/normalise (m/sub pixel origin))]
    (ray origin direction)))

(t/deftest ray-for-camera-test
  (t/testing "center of canvas"
    (let [c (new-camera 201 101 (/ Math/PI 2))
          {:ray/keys [origin direction]} (ray-for-pixel c 100 50)]
      (t/is (rays/=? origin (r/point 0 0 0)))
      (t/is (rays/=? direction (r/vector 0 0 -1)))))
  (t/testing "corner of canvas"
    (let [c (new-camera 201 101 (/ Math/PI 2))
          {:ray/keys [origin direction]} (ray-for-pixel c 0 0)]
      (t/is (rays/=? origin (r/point 0 0 0)))
      (t/is (rays/=? direction (r/vector 0.66519, 0.33259, -0.66851)))))
  (t/testing "camera is transformed"
    (let [c (-> (new-camera 201 101 (/ Math/PI 2))
                (assoc :camera/transform (m/mmul (rm/rotation-y (/ Math/PI 4))
                                                 (rm/translation 0 -2 5))))
          {:ray/keys [origin direction]} (ray-for-pixel c 100 50)]
      (t/is (rays/=? origin (r/point 0 2 -5)))
      (t/is (rays/=? direction (r/vector (m/div (m/sqrt 2) ) 0 (- (m/div (m/sqrt 2) 2))))))))


(defn render [{:camera/keys [hsize vsize] :as c} w]
  (let [img (rc/canvas hsize vsize)]
    (doseq [y (range vsize)]
      (doseq [x (range hsize)]
        (let [r (ray-for-pixel c x y)
              color (color-at w r)]
          (rc/pixel-write! img x y color))))
    img))

(t/deftest render-test
  (t/testing ""
    (let [w (default-world)
          from (r/point 0 0 -5)
          to (r/point 0 0 0)
          up (r/vector 0 1 0)
          c (-> (new-camera 11 11 (/ Math/PI 2))
                (assoc :camera/transform (view-transform from to up)))
          img (render c w)]
      (t/is (rays/=? (rc/pixel-read img 5 5)
                     (rc/color 0.38066 0.47583 0.2855))))))


(defn render-example [w h]
  (let [floor-material (-> (material)
                           (assoc :material/specular 0
                                  :material/color (rc/color 1 0.9 0.9)))
        floor (-> (sphere)
                  (rays/set-transform (rm/scaling 10 0.01 10))
                  (rays/set-material floor-material))
        left-wall (-> (sphere)
                      (rays/set-material floor-material)
                      (rays/set-transform (m/mmul (rm/translation 0 0 5)
                                                  (rm/rotation-y (- (/ Math/PI 4)))
                                                  (rm/rotation-x (/ Math/PI 2))
                                                  (rm/scaling 10 0.01 10))))
        right-wall (-> (sphere)
                       (rays/set-material floor-material)
                       (rays/set-transform (m/mmul (rm/translation 0 0 5)
                                                   (rm/rotation-y (/ Math/PI 4))
                                                   (rm/rotation-x (/ Math/PI 2))
                                                   (rm/scaling 10 0.01 10))))
        middle (-> (sphere)
                   (rays/set-transform (rm/translation -0.5 1 0.5))
                   (rays/set-material (-> (material)
                                          (assoc :material/diffuse 0.7
                                                 :material/specular 0.3
                                                 :material/color (rc/color 0.1 1 0.5)))))
        right (-> (sphere)
                   (rays/set-transform (m/mmul (rm/translation 1.5 0.5 -0.5)
                                               (rm/scaling 0.5 0.5 0.5)))
                   (rays/set-material (-> (material)
                                          (assoc :material/diffuse 0.7
                                                 :material/specular 0.3
                                                 :material/color (rc/color 0.5 1 0.1)))))
        left (-> (sphere)
                   (rays/set-transform (m/mmul (rm/translation -1.5 0.33 -0.75)
                                               (rm/scaling 0.33 0.33 0.33)))
                   (rays/set-material (-> (material)
                                          (assoc :material/diffuse 0.7
                                                 :material/specular 0.3
                                                 :material/color (rc/color 1 0.8 0.1)))))

        world (-> (default-world)
                  (assoc :world/objects [floor left-wall right-wall middle right left]
                         :world/light (point-light (r/point -10 10 -10)
                                                   (rc/color 1 1 1))))
        camera (-> (new-camera w h (/ Math/PI 3))
                   (assoc :camera/transform (view-transform (r/point 0 1.5 -5)
                                                            (r/point 0 1 0)
                                                            (r/vector 0 1 0))))]
    (render camera world)))

(defn render-default [w h]
  (let [world (default-world)
        camera (-> (new-camera w h (/ Math/PI 4))
                   (assoc :camera/transform (view-transform (r/point 5 4.5 -1)
                                                            (r/point 0 0 0)
                                                            (r/vector 0 1 0))))]
    (render camera world)))
(comment
  (-> (render-default 111 110)
      (rc/canvas-str)
      (->> (spit "world-default.ppm"))))
(comment
  (require '[clj-async-profiler.core :as prof])

  (time
   (-> (render-example 600 300)
       (rc/canvas-str)
       (->> (spit "world.ppm"))))
  (prof/serve-files 7001)
  (prof/profile
   (-> (render-example 640 480)
       (rc/canvas-str)
       (->> (spit "world.ppm")))))