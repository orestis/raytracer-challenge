(ns raytracer.canvas
  (:require [raytracer.core :as r]
            [clojure.core.matrix :as m]
            [clojure.test :as t]
            [clojure.string :as string]))

(defn red [c]
  (m/mget c 0))

(defn green [c]
  (m/mget c 1))

(defn blue [c]
  (m/mget c 2))

(defn color [& args]
  (m/array args))


(t/deftest colors
  (let [c (color -0.5 0.4 1.7)]
    (t/is (= -0.5 (red c)))
    (t/is (= 0.4 (green c)))
    (t/is (= 1.7 (blue c)))))


(t/deftest colors-ops
  (t/is (= (color 1.6 0.7 1.0)
           (r/+ (color 0.9 0.6 0.75)
                (color 0.7 0.1 0.25))))
  (t/is (m/equals (color 0.9 0.2 0.04)
                  (r/* (color 1 0.2 0.4)
                          (color 0.9 1 0.1))
                  0.00001)))


(defn canvas [w h]
  (m/zero-array [w h 3]))

(defn width [cnv]
  (first (m/shape cnv)))

(defn height [cnv]
  (second (m/shape cnv)))

(defn pixel-seq [cnv]
  (for [x (range (width cnv))
        y (range (height cnv))]
    (m/select cnv x y :all)))

(t/deftest canvas-test
  (let [cnv (canvas 20 10)]
    (t/is (= 20 (width cnv)))
    (t/is (= 10 (height cnv)))
    (doseq [p (pixel-seq cnv)]
      (t/is (= (color 0 0 0)
               p)))))


(defn pixel-write [cnv x y c]
  (m/set-selection cnv x y :all c))

(defn pixel-write! [cnv x y c]
  (m/set-selection! cnv x y :all c))

(defn pixel-read [cnv x y]
  (m/select cnv x y :all))

(t/deftest canvas-ops
  (let [cnv (canvas 10 20)
        red (color 1 0 0)
        cnv' (pixel-write cnv 2 3 red)]
    (t/is
     (= (pixel-read cnv' 2 3)
        red))))

(defn canvas-str [cnv]
  (let [sb (StringBuilder.)
        w (dec (* 3 (width cnv)))
        ppm (-> cnv
                (r/* 255)
                (m/round))
        sq (m/eseq ppm)]
    (.append sb "P3\n")
    (.append sb (str (width cnv) " " (height cnv) "\n"))
    (.append sb "255")
    (loop  [p (first sq)
            sq (rest sq)
            x 0
            line-length 0]
      (let [eol (= x w)
            p (str (int (m/clamp p 0 255)))
            line-length' (+ line-length (inc (count p)))
            ;; adding this string would overflow
            would-overflow? (> line-length' 70)
            add-space? (not (or (= x 0) would-overflow?))
            line-length' (if add-space?
                           line-length'
                           (count p))]
        (if (not add-space?)
          (.append sb "\n")
          (.append sb " "))
        (.append sb p)
        (when (seq sq)
          (recur (first sq) (rest sq)
                 (if eol 0 (inc x))
                 line-length'))))
    (.append sb "\n")
    (str sb)))

(t/deftest canvas-header
  (let [cnv (canvas 5 3)
        s (canvas-str cnv)
        lines (take 3 (string/split-lines s))]
    (t/is (= lines
             ["P3" "5 3" "255"]))))


(t/deftest canvas-data
  (let [[c1 c2 c3] [(color 1.5 0 0)
                    (color 0 0.5 0)
                    (color -0.5 0 1)]
        cnv (-> (canvas 5 3)
                (pixel-write 0 0 c1)
                (pixel-write 2 1 c2)
                (pixel-write 4 2 c3))
        s (canvas-str cnv)
        lines (drop 3 (string/split-lines s))]
    (t/is (= lines
             ["255 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
              "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0"
              "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"]))))

(defn fill-canvas [cnv c]
  (let [cnv' (atom cnv)]
    (doseq [x (range (width cnv))
            y (range (height cnv))]
      (swap! cnv' pixel-write! x y c))
    @cnv'))

(t/deftest canvas-line-lengt
  (let [cnv (fill-canvas (canvas 10 2)
                         (color 1 0.8 0.6))
        s (canvas-str cnv)
        lines (drop 3 (string/split-lines s))]
    (t/is (= lines
             ["255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
              "153 255 204 153 255 204 153 255 204 153 255 204 153"
              "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
              "153 255 204 153 255 204 153 255 204 153 255 204 153"]))))