(ns riFF.riFF
  (:require [overtone.live :refer :all :exclude [stop sharp flat]]
            [leipzig.melody :refer :all]
            [leipzig.canon :refer [canon interval]]
            [leipzig.scale :refer [high low raise lower sharp flat E B G minor major from]]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [overtone.inst.drum :as drums]
            [riFF.instruments :as inst]))

(def part (partial all :part))

(defn vary [f notes]
  (->> notes
       (then (f notes))))

(defn reps [& repetitions]
  (mapcat (partial apply repeat) repetitions))

(defn silence [duration]
  (phrase [duration] [nil]))

(def riFF
  (let [seven (->> [0 0 2 0 -1 -2 -3 -1]
                   (phrase [1.5 0.5 0.75 0.75 0.5 2 2])
                   ;(vary #(but 4 5.5 (phrase [0.75 0.75 0.5] [-2 -1 -2]) %))
                   (times 8)
                   ;(where :pitch raise)
                   (where :pitch lower)
                   (part :riff))
        nation-army (->> (phrase (reps [13 1/2] [2 3/4] [1 1/2])
                                 [0 4 4 3 3 2 2 1 1 0 0 -1 -1 0 0 0])
                         (after -1/2)
                         ;(phrase (repeat 1) (concat (range 4 -2 -1) [0 0]))
                         (then (silence 8))
                         (times 2)
                         (after 32)
                         (part :melody))
        rise (->> (reps [8 [-2 2 5]] [8 [-1 3 6]])
                  (phrase (repeat 16 0.5))
                  (with (phrase [4 4] [[-9 -2] [-8 -1]]))
                  (where :pitch lower)
                  (part :riff))
        sweet (->> [0 0 [0 7] 0
                    2 [2 9] 0 [0 7]
                    -2 -2 -2 [0 7]
                    -3 -3 -3 [0 7]]
                   (phrase (repeat 0.5))
                   (times 4)
                   (part :bop))
        dreams (->> [nil 2 2 0 2 1]
                    (phrase [1 1 1 1 2 2])
                    (times 3)
                    (but 12 14 (phrase [0.5 1.5] [2 3]))
                    (then (phrase [2.0 0.5 1.5 0.5 0.5 0.5 1.0 1.5]
                                  [2 2 0 2 2 3 2 1]))
                    (where :pitch raise)
                    (part :melody))
        bass (->> [0 -2 -3]
                  (phrase [4 2 2])
                  ;(canon #(where :pitch raise %))
                  (where :pitch (comp lower lower))
                  (times 2))
        beat (->> (repeat 8 1)
                  rhythm
                  (having :part (cycle [:kick :snare])) (times 2))]
    (->> (with seven nation-army)
         (with (times 2 (with bass beat)))
         ;(with sweet dreams)
         (then rise)
         (where :pitch (comp G minor))
         (tempo (bpm 120)))))

(comment
  (volume 0.3)
  (live/jam (var riFF))
  (def riFF nil))

(comment
  (map fx-chorus [0 1])
  (map fx-distortion [0 1] [2 2] [0.18 0.14])

  (do (recording-start "geb.aiff")
      (live/play geb))
  (recording-stop))

; Instrumentation
(defonce x
  (defsynth walker [out-bus 0 freq 0.5]
    (out:kr out-bus (lf-noise1:kr freq))))
(defonce random-walk (audio-bus))
(defonce walk (walker random-walk :freq (* 1/7 0.75)))
(def resonance (mul-add (in:kr random-walk) 2500 3000))

(definst sweetar [freq 110 dur 1.0 boost 5 vol 0.25 pan 0.0]
  (let [inst (-> (sin-osc freq)
                 (+ (* 4 (sin-osc (* 0.999 freq))))
                 (+ (* 1 (sin-osc (* 1.5 freq))))
                 (+ (* 2 (sin-osc (* 4.003 freq))))
                 (+ (* 2 (sin-osc (* 0.501 freq))))
                 (* boost)
                 (clip2 0.7)
                 (rlpf resonance 0.7)
                 (pan2 pan)
                 (* (env-gen (adsr 0.03 0.2 0.3 0.1)
                             (line:kr 1 0 dur) :action FREE))
                 (* vol))
        delayed (delay-l inst 0.001)
        reverbed (free-verb delayed :damp 0.1 :mix 0.1 :room 0.2)
        dryverbed (free-verb inst :damp 0.3 :mix 0.1 :room 0.2)]
    (mix reverbed dryverbed)))

(definst sing [freq 110 dur 1.0 boost 5 vol 0.25 pan 0.0]
  (let [inst (-> (sin-osc freq)
                 (+ (* 2 (saw (* 0.9999 freq))))
                 (+ (* 1 (sin-osc (* 0.501 freq))))
                 (* boost)
                 (clip2 0.8)
                 (rlpf (line:ar 5000 3000 dur) 0.4)
                 (rlpf resonance 0.5)
                 (pan2 pan)
                 (* (env-gen (adsr 0.05 0.2 0.8 0.1)
                             (line:kr 1 0 dur) :action FREE))
                 (* vol))
        delayed (delay-l inst 0.001)
        reverbed (free-verb delayed :damp 0.4 :mix 0.1 :room 0.2)
        dryverbed (free-verb inst :damp 0.2 :mix 0.1 :room 0.2)]
    (mix reverbed dryverbed)))

(definst bop [freq 110 dur 1.0 vol 0.25 pan 0.0]
  (let [inst (-> (saw freq)
                 (+ (saw (* 1/2 0.999 freq)))
                 (rlpf (line:kr 8000 100 dur) 0.7)
                 (* 2)
                 (pan2 pan)
                 (* (env-gen (perc 0.00 0.9) (line:kr 1 0 dur) :action FREE))
                 (* vol))
        delayed (delay-l inst 0.01)
        reverbed (free-verb delayed :damp 0.4 :mix 0.1 :room 0.2)
        dryverbed (free-verb inst :damp 0.2 :mix 0.1 :room 0.2)]
    (mix reverbed dryverbed)))

(defmethod live/play-note :default
  [{midi :pitch seconds :duration}]
  (-> midi midi->hz (sweetar seconds)))

(defmethod live/play-note :riff
  [{midi :pitch seconds :duration}]
  (-> midi midi->hz (sweetar seconds :vol 0.5 :pan -1.5 :boost 109)))

(defmethod live/play-note :bop
  [{midi :pitch seconds :duration}]
  (-> midi midi->hz (bop seconds :pan -0.3 :boost 10)))

(defmethod live/play-note :melody
  [{midi :pitch seconds :duration}]
  (-> midi midi->hz (sing seconds :vol 0.2 :pan 0.5 :boost 1)))

(defmethod live/play-note :kick
  [{midi :pitch seconds :duration}]
  (drums/kick2))

(defmethod live/play-note :snare
  [{midi :pitch seconds :duration}]
  (drums/hat3))
