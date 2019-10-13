(ns jazz.standards.in-the-mood
  (:use leipzig.scale, leipzig.melody, leipzig.chord, leipzig.live jazz.instruments))

; Gracias a Wingy Manone, Andy Razaf y Joe Garland.

(def in-the-mood
  (let [bass #(->> (phrase [1 1 1 1/2 1/2 1 1 1 1] [0 2 4 5 4 7 5 4 2])
                   (where :pitch (from %))
                   (where :pitch (comp lower lower))
                   (cut 8))
        hook #(->> %
                   vals sort cycle
                   ;repeat
                   (phrase (concat (repeat 11 1/2) [5/2]))
                   (cut 8))
        beat (->> [1 1/2 1/2] cycle rhythm (cut (* 6 8)) (all :part :beat))]
    (->> (mapthen bass [0 0 3 0 4 0])
         (with (mapthen hook
                        [(-> triad (inversion 1) (root 7))
                         (-> triad (inversion 1) (root 7))
                         (-> triad (root 3))
                         (-> triad (inversion 1) (root 7))
                         (-> triad (root 4))
                         (-> triad (inversion 1) (root 7))]))
         (with beat)
         (where :pitch (comp low B flat major))
         (tempo (comp (scale [2/3 1/3]) (partial * 2)))
         (tempo (bpm 120)))))

(comment
  (-> in-the-mood var jam)
  )
