(ns mandelbrot.core
  (:import [java.awt Color Graphics Graphics2D]
           [java.awt.image BufferedImage])
  (:require [seesaw.core :refer :all]
            [seesaw.dev :refer :all]
            [seesaw.mig :refer :all]))

(def colors
  {:gunmetal (Color. 42 52 57)})

(def canvas-args
  {:width 1000 :height 1000})

(def max-iter (atom 25))
(def square-scale 4)
(def nsquares (/ (canvas-args :width) square-scale))

(def img (BufferedImage. (canvas-args :width) (canvas-args :height) BufferedImage/TYPE_INT_ARGB))

(def cam-width 4.0)
(def cam (atom {:x 0 :y 0 :width cam-width}))

(defn move-cam [dx dy]
  (let [sx (* dx (/ (:width @cam) 20))
        sy (* dy (/ (:width @cam) 20))]
    (swap! cam #(-> %
                    (update :x + sx)
                    (update :y + sy)))))

(defn zoom-cam [factor]
  (swap! cam #(update % :width * factor)))


(defn reset-cam []
  (reset! cam {:x 0 :y 0 :width cam-width})
  (reset! max-iter 25))

(defn pixel->unit [x y]
  [(/ x nsquares)
   (/ y nsquares)])

(defn unit->complex [x y]
  (let [width (:width @cam)
        cx (:x @cam)
        cy (:y @cam)]
    [(+ (- cx (/ width 2)) (* width x))
     (+ (- cy (/ width 2)) (* width y))]))

(defn pixel->complex [x y]
  (apply unit->complex (pixel->unit x y)))

(defn mandelbrot? [^double x ^double y]
  (loop [zr 0.0
         zi 0.0
         i 0]
    (let [zr2 (* zr zr)
          zi2 (* zi zi)]
      (if (or (> zr2 4.0) (> (+ zr2 zi2) 4.0))
        i
        (if (>= i @max-iter)
          -1
          (let [new-zr (+ (- zr2 zi2) x)
                new-zi (+ (* 2 zr zi) y)]
            (recur new-zr new-zi (inc i))))))))

(defn iter->color [iter]
  (if (= iter -1)
    Color/BLACK
    (let [norm (/ (double iter) @max-iter)
          hue (* 360.0 norm)
          saturation 1.0
          brightness 1.0]
      (Color/getHSBColor (/ hue 360.0) saturation brightness))))

(defn draw-mandelbrot-square! [^Graphics g x y color]
  (doto g
    (.setColor color)
    (.fillRect (* x square-scale)
               (* y square-scale)
               square-scale
               square-scale)))

(defn mandelbrot []
  (invoke-later
    (let [paint (fn [c ^Graphics g] (.drawImage g img 0 0 nil))
          canvas (canvas :paint paint
                         :preferred-size [(canvas-args :width) :by (canvas-args :height)])
          panel (mig-panel :constraints ["" "[grow]" "[grow][]"]
                           :background (colors :gunmetal)
                           :items [[canvas "cell 0 0, align center center, gap 0 20"]])
          frame (frame :title "Mandelbrot Viewer"
                       :content panel
                       :on-close :exit)]

      (defn paint-set []
        (let [^Graphics2D g (.getGraphics img)]
          (doseq [x (range nsquares)
                  y (range nsquares)]
            (let [[cx cy] (pixel->complex x y)
                  iter (mandelbrot? cx cy)
                  color (iter->color iter)]
              (draw-mandelbrot-square! g x y color)))
          (.dispose g)
          (repaint! canvas)))

      (listen frame :key-pressed
              (fn [e]
                (case (.getKeyChar e)
                  \r (do (reset-cam) (paint-set))
                  \h (do (move-cam -1 0) (paint-set))
                  \j (do (move-cam 0 1) (paint-set))
                  \k (do (move-cam 0 -1) (paint-set))
                  \l (do (move-cam 1 0) (paint-set))
                  \u (do (zoom-cam 0.9) (paint-set))
                  \i (do (zoom-cam 1.1) (paint-set))
                  \n (do (swap! max-iter + 1) (paint-set))
                  \m (do (swap! max-iter - 1) (paint-set))
                  nil)))

      (paint-set)
      (doto frame
        (.setFocusable true)
        (.requestFocusInWindow)
        (pack!)
        (show!)))))

(defn -main
  [& args]
  (mandelbrot))


