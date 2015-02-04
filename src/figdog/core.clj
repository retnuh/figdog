(ns figdog.core
  (:import (clojure.lang PersistentQueue))
  (:require  [clojure.zip :as zip]
             [clojure.java.io :as io]
             [clojure.string :as str])
  (:use [clojure.set] [criterium.core]))

(defn okay-word? [word len]
  (and
    (= (count word) len)
    (every? #(Character/isLowerCase %) word)
    ))

(defn read-lines [resource]
  (str/split (slurp  (io/file (io/resource resource))) #"\s+"))

(def dictionary (read-lines "wordsEn.txt"))

(defn load-words [len]
  (let [filtered-words (filter #(okay-word? % len) dictionary)]
    (set filtered-words)))

(defn load-words-t [len]
  (into #{} (filter #(okay-word? % len)) dictionary))


;(make-words #{"fig" "foo"} "fog")

(defn print-path
  ([words]
    (let [word (first words)
          w (rest (reverse (flatten (rest words))))]
      (print-path word w)))
  ([word path]
    (doall (for [w path]
             (print w "-> ")))
    (and word (println word))))

(def empty-q PersistentQueue/EMPTY)

#_(defn print-path-lazy [start-word end-word]
  (let [[path count] (time (find-path-lazy start-word end-word))
        pp (fn [v])]
    (print-path end-word (map first (zip/path path)))
    (println count " rounds")))


(println "core loaded")
;
;(println (time (find-path "fig" "dog")))
;(print-path-zipper "fig" "dog")
;(println (time (find-path "fig" "xxx")))
;(println (time (find-path "pearl" "cream")))
;(println (time (find-path-lazy "pearl" "cream")))
;(print-path-zipper "pearl" "cream")
;(println (time (find-path "pearl" "water")))
;(print-path-zipper "pearl" "water")
;(quick-bench (load-words 5))
;(quick-bench (load-words-t 5))
