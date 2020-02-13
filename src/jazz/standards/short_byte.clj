(ns jazz.standards.short-byte
  (:use leipzig.scale, leipzig.melody, leipzig.live, leipzig.chord, leipzig.temperament, leipzig.canon
        jazz.instruments
        [overtone.live :only [now]]
        [quil.core :only
         [color clear smooth sketch ellipse frame-rate background
          width height stroke stroke-weight fill screen-height screen-width]]))

(def tonic triad)
(def third (-> triad (root 2) (inversion 2)))
(def fourth (-> triad (root 3) (inversion 2)))
(def fifth (-> triad (root 4) (inversion 1)))

(def bar 8)

(def short-byte
  (let [bassline (->> [0 2 4 3 2 3 5 4] (phrase [2/4 2/4 3/4 2/4 2/4 2/4 1/4 14/4])
                      (then (->> [0 2 4 3 5 4 3 2 1] (phrase [2/4 2/4 3/4 2/4 2/4 2/4 1/4 7/4 7/4])))
                      )
        beat (->> (rhythm [2/4 2/4 2/4 1/4])
                  (times 4)
                  (all :part :beat)
                  (with (->> (times 2 (rhythm [1/2 1/2 1/4 1/4 1/2 1/2 1/2 1/4 1/4])) (all :part :kick)))
                  (times 2))
        chord (fn [c] (->> c vals reverse cycle (phrase [4/4 1/8 1/8 1/2])))
        sigh (phrase (repeat 56 1/4) (cycle [7 6 4]))
        face (phrase (repeat 28 1/2) (cycle [11 11 12 13 9 14 13]))
        few (->> (phrase (cycle [2/4 2/4 1/8 1/8 1/4 1/4]) (repeat [-7 0 7 11 14])) (cut 14))
        sorta (->> (phrase (repeat 1/4) [0 1 2 0 4 0 4]))]
    (->> bassline
         (where :pitch (comp lower lower))
         (with #_chords beat #_sigh #_face few #_sorta)
         ;(with (phrase [14] [0]))
         (with (mapthen chord [tonic fourth fifth fifth tonic fourth third fifth]))
      (where :pitch (comp A major))
      (tempo (bpm 80))
         )))

(comment
  (do (stop) (-> short-byte var jam))
  (def short-byte nil)
)
