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
  (m/mset! cnv x y 0 (m/mget c 0))
  (m/mset! cnv x y 1 (m/mget c 1))
  (m/mset! cnv x y 2 (m/mget c 2))
  #_
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

(defn- format-ppm [x]
  (str (int (m/clamp x 0 255))))

(defn- write-v [^StringBuilder sb s add-space?]
  (when add-space?
    (.append sb " "))
  (.append sb s)
  (if add-space?
    (inc (count s))
    (count s)))

(defn would-overflow?
  [line-length n]
  (> (+ line-length n) 70))

(defn canvas-str [cnv]
  (let [sb (StringBuilder. (int (+ 1024 (* (width cnv) (height cnv) 4))))
        max-x (dec (width cnv))
        ppm (-> cnv
                (r/* 255)
                (m/round))]
    (.append sb "P3\n")
    (.append sb (str (width cnv) " " (height cnv) "\n"))
    (.append sb "255")
    (doseq [y (range (height cnv))
            :let [line-length (atom 0)]]
      ;; on every new row, break the line
      (.append sb "\n")
      (loop [x 0]
        (loop [z 0]
          (let [pp (m/mget ppm x y z)
                s (format-ppm pp)
                overflow? (would-overflow? @line-length (inc (count s)))
                add-space? (not (or overflow? (= @line-length 0)))]
            (if overflow?
              (do
                (.append sb "\n")
                (reset! line-length (count s)))
              (swap! line-length + (inc (count s))))
            (write-v sb s add-space?))
          (when (< z 2)
            (recur (inc z))))
        (when (< x max-x)
          (recur (inc x))))
      #_(doseq [x (range (width cnv))
                z (range 3)]
          (let [pixel (pixel-read ppm x y)]
            (let [pp (m/mget ppm x y z); (m/eseq pixel)
                  s (format-ppm pp)
                  overflow? (would-overflow? @line-length (inc (count s)))
                  add-space? (not (or overflow? (= @line-length 0)))]
              (if overflow?
                (do
                  (.append sb "\n")
                  (reset! line-length (count s)))
                (swap! line-length + (inc (count s))))
              (write-v sb s add-space?)))))
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