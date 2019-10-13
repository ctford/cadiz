(ns jazz.standards.was-in-the-mood
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord, leipzig.temperament
        jazz.instruments
        [overtone.live :only [now]]
        [quil.core :only
         [color clear smooth sketch ellipse frame-rate background
          width height stroke stroke-weight fill screen-height screen-width]]))

(def tonic (-> triad (root 7) (inversion 1)))
(def fourth (-> triad (root 3)))
(def fifth (-> triad (root 4)))

(def bar 8)

(def in-the-mood
  (let [bassline (fn [root]
                   (->> [0 2 4 5 4 7 5 4 2]
                        (phrase [1 1 1 1/2 1/2 1 1 1 1])
                        (cut bar)
                        (where :pitch (comp lower lower))
                        (where :pitch (from root))))
        hook (fn [chord]
               (->> chord
                    vals sort cycle
                    ;repeat
                    (phrase (concat (repeat 11 1/2) [5/2]))
                    (cut bar)))
        beat (->> (rhythm (cycle [1 1/2 1/2]))
                  (cut bar)
                  (all :part :beat))]
    (->>
      (mapthen bassline [0 0 3 0 4 0])
      (with (mapthen hook [tonic tonic fourth tonic fifth tonic]))
      (with (times 6 beat))
      (tempo (comp (scale [2/3 1/3]) (partial * 2)))
      (where :pitch (comp C major))
      (tempo (bpm 120)))))

(comment
  (jam (var in-the-mood) play-at)
  (def in-the-mood nil)
)

(defonce hits (atom []))
(defonce plinks (atom []))

(comment
  (do
    (reset! hits [])
    (reset! plinks []))

  (sketch
    :setup (fn [] (stroke-weight 5))
    :draw  (fn []
             (let [current (now)
                   hits-played (->> hits deref (filter #(< % current)))
                   plinks-played (->> plinks deref (filter #(< (:time %) current)))]
               (clear)
               (fill (color 135 4 55))
               (doseq [epoch hits-played]
                 (ellipse
                   (mod (/ epoch 30) (screen-width))
                    (+ 100 (/ (- current epoch) 20))
                   50
                   50))
               (fill (color 234 123 28))
               (doseq [note plinks-played]
                 (ellipse
                   (mod (+ 350 (* 5 (- (:pitch note) 50))) 923)
                   (+ 50 (/ (- current (:time note)) 20))
                   10
                   (* 100 (:duration note))))))
    :size [(screen-width) (screen-height)]))

(defmethod play-note :beat [{epoch :time}]
  (swap! hits conj epoch)
  (hat))

(defmethod play-note :default
  [{midi :pitch, seconds :duration :as note}]
  (swap! plinks conj note)
  (-> midi (piano :duration seconds)))
