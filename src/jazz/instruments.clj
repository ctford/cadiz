(ns jazz.instruments
  (:use [overtone.core]
        [leipzig.live :only [play-note]]
        [overtone.samples.piano :only [index-buffer]]))

(definst hat []
  (let [buffer (load-sample (freesound-path 802))]
    (-> (play-buf 1 buffer :action FREE)
        #_(* (env-gen (perc 0.01 0.1) :action FREE)))))

(definst piano
  [note 60 level 1 rate 1 loop? 0 duration 8
   attack 0 decay 1 sustain 1 release 0.1 curve -4 gate 1]
  (let [buf (index:kr (:id index-buffer) note)]
    (-> (scaled-play-buf 2 buf :rate rate :level level :loop loop? :action FREE)
        ;(rhpf (line:kr 1000 200 duration) 0.5)
        ;(* 4) (clip2 0.6) (lpf 1500)
        (* (env-gen (adsr attack decay sustain release level curve)
                    (line:kr 1 0 duration)
                    :action FREE)))))

(defmethod play-note :default
  [{midi :pitch, seconds :duration}]
  (piano midi :duration seconds))

(defmethod play-note :beat
  [_]
  (hat))
