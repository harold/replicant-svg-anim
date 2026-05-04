(ns app.core
  (:refer-clojure :exclude [+ *])
  (:require [clojure.core :as cc]
            [replicant.dom :as d]))

(defn +
  ([x y] (if (number? x) (cc/+ x y) (mapv + x y)))
  ([x y & more] (reduce + (+ x y) more)))

(defn * [x y]
  (if (number? y) (cc/* x y) (mapv #(* x %) y)))

(defn rot [θ u v]
  (let [c (Math/cos θ)
        s (Math/sin θ)]
    [(+ (*    c  u) (* s v))
     (+ (* (- s) u) (* c v))]))

(def vertices
  ;; Tesseract: all 16 sign combinations of +/-1/2 in 4D.
  ;; Outer loop is w, so 0-7 fill the w=-1/2 cube and 8-15 the w=+1/2 cube.
  (vec (for [w [-0.5 0.5]
             z [-0.5 0.5]
             y [-0.5 0.5]
             x [-0.5 0.5]]
         [x y z w])))

(def edges
  ;; Two vertices are connected iff they differ in exactly one coordinate,
  ;; i.e. their indices differ in exactly one bit.
  (vec (for [i (range 16)
             b [1 2 4 8]
             :let [j (bit-xor i b)]
             :when (< i j)]
         [i j])))

(defn to-world [basis coeffs]
  (apply + (map * coeffs basis)))

(defn perspective-drop [offset v]
  (* (/ 1 (+ (peek v) offset)) (pop v)))

(defn edge-color [[i j]]
  (cond
    (< j 8)  "#22c55e"   ;; w = -1/2 cube
    (>= i 8) "#60a5fa"   ;; w = +1/2 cube
    :else    "#f59e0b")) ;; connector across the w-axis

(defn view [{:keys [basis]}]
  (let [project (comp #(+ [250 250] (* 800 %))
                      (partial perspective-drop 2)
                      (partial perspective-drop 2.5)
                      (partial to-world basis))
        pts (mapv project vertices)]
    [:svg {:viewBox "0 0 500 500"}
     (for [[i j :as e] edges
           :let [[x1 y1] (pts i)
                 [x2 y2] (pts j)]]
       [:line {:key            (str i "-" j)
               :x1 x1 :y1 y1 :x2 x2 :y2 y2
               :stroke         (edge-color e)
               :stroke-width   2
               :stroke-linecap "round"}])]))

(defn tumble [[b1 b2 b3 b4] t1 t2 t3]
  (let [[b1 b2] (rot t1 b1 b2)
        [b3 b4] (rot t2 b3 b4)   ;; independent plane -- a true 4D double rotation
        [b2 b3] (rot t3 b2 b3)]
    [b1 b2 b3 b4]))

(defn step [basis]
  (tumble basis 0.003 0.005 0.002))

(defonce state*
  (atom {:basis (let [r #(cc/* 2 Math/PI (rand))]
                  (tumble [[1 0 0 0] [0 1 0 0] [0 0 1 0] [0 0 0 1]]
                          (r) (r) (r)))}))

(defn tick [_ts]
  (swap! state* update :basis step)
  (d/render (js/document.getElementById "app") (view @state*))
  (js/requestAnimationFrame tick))

(defn init! []
  (js/requestAnimationFrame tick))
