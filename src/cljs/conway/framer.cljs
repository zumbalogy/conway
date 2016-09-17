(ns conway.framer
  (:require [reagent.core :as reagent :refer [atom]]))

(defonce images (atom []))
(defonce image-index (atom 0))

(defn frame []
  [:div
   [:canvas#canvas {:width 300 :height 300}]
   [:br]
   (if-let [img (get @images (mod @image-index (count @images)))]
     [:img {:src (.-src img)}])
   ])

(defn get-ctx [canvas]
  (.getContext canvas "2d"))

(defn pixel-data [canvas]
  (let [h (.-height canvas)
        w (.-width canvas)
        ctx (get-ctx canvas)]
  (.-data (.getImageData ctx 0 0 w h))))

(defn neighbors [data index width]
  (let [w4 (* 0.1 width)]
    [(aget data (- index 4))
     (aget data (+ index 4))
     (aget data (+ index w4))
     (aget data (- index w4))
     (aget data (- index w4 4))
     (aget data (+ index w4 5))
     (aget data (+ (- index w4) 4))
     (aget data (- (+ index w4) 4))]))

(defn conway [data index w]
  (let [c (aget data index)
        neighbors (neighbors data index w)
        amount (count (filter #(< 130 %) neighbors))]
      (cond
        (= 3 amount) (apply max neighbors)
        (= 2 amount) c
        :else (apply min neighbors))))

(defn draw [data canvas]
  (let [h (.-height canvas)
        w (.-width canvas)
        ctx (get-ctx canvas)
        img (.createImageData ctx w h)
        pixels (.-data img)]
    (doseq [x (range 0 (* w h 4) 4)]
      (aset pixels x (conway data x w))
      (aset pixels (+ 1 x) (conway data (+ 1 x) w))
      (aset pixels (+ 2 x) (conway data (+ 2 x) w))
      (aset pixels (+ 3 x) 255)
      )
    (.putImageData ctx img 0 0)))

(defn tick [canvas]
  (draw (pixel-data canvas) canvas))

(defn get-image [canvas]
  (let [image (js/Image.)]
    (set! (.-src image) (.toDataURL canvas "image/png"))
    image))

(defn setup []
  (let [canvas (js/document.getElementById "canvas")
        ctx (get-ctx canvas)
        img (js/Image.)
        interval (fn [] (do
          (tick canvas)
          (swap! images conj (get-image canvas))
          ))]
    (set! (.-onload img) #(.drawImage ctx img 0 0))
    (set! (.-src img) "/images/beta.jpg")
    ; (js/setInterval interval 100)
    (dotimes [n 30]
      (js/setTimeout interval (* 110 n)))
    (js/setInterval #(swap! image-index inc) 150)
    ))

    ; 150 zoom, lower right corner of monitor screen
    ; byzanz-record --duration=5 --x=200 --y=974 --width=400 --height=450 out.gif
