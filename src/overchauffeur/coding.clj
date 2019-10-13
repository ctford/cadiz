(ns overchauffeur.coding
  (:require [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]))

(defmacro defs [names values]
  `(do
     ~@(map
         (fn [name value] `(def ~name ~value))
         names (eval values))))

(def char->ascii int)

(defs [A B C D E F G]
  (map
    (comp scale/A scale/low scale/minor)
    (range)))

(defs [a b c d e f g]
  (map
    int
    "ABCDEFG"))

(defn ascii->midi [n]
  (->> n
       char
       str
       (symbol "overchauffeur.coding")
       find-var
       deref))

(def midi->ascii
  {A 65
   B 66
   C 67
   D 68
   E 69
   F 70
   G 71})
