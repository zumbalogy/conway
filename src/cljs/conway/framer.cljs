(ns conway.framer)

(defn frame []
   [:canvas#canvas {:width 300 :height 300}])

(defn get-ctx [canvas]
  (.getContext canvas "2d"))

(defn pixel-data [canvas]
  (let [h (.-height canvas)
        w (.-width canvas)
        ctx (get-ctx canvas)]
  (.-data (.getImageData ctx 0 0 w h))))

(defn neighbors [data index width]
  (let [w4 (* 4 width)]
    [(aget data (- index 4))
     (aget data (+ index 4))
     (aget data (+ index w4))
     (aget data (- index w4))
     (aget data (- index w4 4))
     (aget data (+ index w4 4))
     (aget data (+ (- index w4) 4))
     (aget data (- (+ index w4) 4))
     ]))

(defn conway [data index w]
  (let [c (aget data index)
        neighbors (neighbors data index w)
        amount (count (filter #(< 200 %) neighbors))]
      (cond
        true (rand-nth neighbors)
        (< 0 amount 8) (+ (apply max neighbors) 10)
        :else (- c 10))))

(defn draw [data canvas]
  (let [h (.-height canvas)
        w (.-width canvas)
        ctx (get-ctx canvas)
        img (.createImageData ctx w h)
        pixels (.-data img)]
    (doseq [x (range (* 4 w) 280000 4)]
      (aset pixels x (conway data x w))
      (aset pixels (+ 1 x) (conway data (+ 1 x) w))
      (aset pixels (+ 2 x) (conway data (+ 2 x) w))
      (aset pixels (+ 3 x) 255))
    (.putImageData ctx img 0 0)))

(defn tick [canvas]
  (draw (pixel-data canvas) canvas))

(defn setup []
  (let [canvas (js/document.getElementById "canvas")
        ctx (get-ctx canvas)
        img (js/Image.)]
    (set! (.-onload img) #(.drawImage ctx img 0 0))
    (set! (.-src img) "/images/beta.jpg")
    (js/setInterval #(tick canvas) 100)))
