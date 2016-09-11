(ns conway.framer
    (:require [reagent.core :as reagent :refer [atom]]))

(defn frame []
   [:canvas#canvas {:width 300 :height 300}])

(defn get-ctx [canvas]
  (.getContext canvas "2d"))

(defn pixel-data [canvas]
  (let [h (.-height canvas)
        w (.-width canvas)
        ctx (get-ctx canvas)]
  (.-data (.getImageData ctx 0 0 w h))))

(defn rows [canvas]
  (partition (* 4 (.-width canvas)) (pixel-data canvas)))

(defn neighbors [data index width]
  [(aget data (- index 4))
   (aget data (+ index 4))
   (aget data (+ index width))
   (aget data (- index width))
   (aget data (- index width 4))
   (aget data (+ index width 4))
   (aget data (+ (- index width) 4))
   (aget data (- (+ index width) 4))])

(defn draw [data canvas]
  (let [h (.-height canvas)
        w (.-width canvas)
        ctx (get-ctx canvas)
        img (.createImageData ctx w h)
        pixels (.-data img)]
    (doseq [x (range 0 280000 4)]
      (aset pixels x
        (let [c (aget data x)
              neighbors (neighbors data x w)
              amount (count (filter #(< 120 %) neighbors))]
            (max 0 (cond
              (= 3 amount) (+ (apply max neighbors) 10)
              :else (- c 10)))
          ))
      (aset pixels (+ 1 x)
        (let [c (aget data (+ 1 x))
              neighbors (neighbors data (+ 1 x) w)
              amount (count (filter #(< 120 %) neighbors))]
            (max 0 (cond
              (= 3 amount) (+ (apply max neighbors) 10)
              :else (- c 10)))
          ))
      ; (aset pixels (+ 1 x) (+ 0 (aget data (+ 1 x))))  ; g
      (aset pixels (+ 2 x) (+ 0 (aget data (+ 2 x))))  ; b
      (aset pixels (+ 3 x) (+ 0 (aget data (+ 3 x))))) ; a
    (.putImageData ctx img 0 0)))


(defn tick [canvas]
  (draw (pixel-data canvas) canvas))

(defn setup []
  (let [canvas (js/document.getElementById "canvas")
        ctx (get-ctx canvas)
        img (js/Image.)]
    (set! (.-onload img) #(.drawImage ctx img 0 0))
    (set! (.-src img) "/images/pencils.png")
    (js/setInterval #(tick canvas) 400)
    ))
