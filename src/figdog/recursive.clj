(ns figdog.recursive
  (:require [figdog.lazy :as lazy]
            [figdog.transducer :as transducer])
  (:use [clojure.set] [criterium.core] [figdog.core]))

(defn find-path-recur
  ([start-word end-word]
    (find-path-recur start-word end-word lazy/make-words))
  ([start-word end-word make-fn]
    (let [all-words (load-words (count start-word))
          all-words-count (count all-words)]
      (loop [todo (conj empty-q [start-word []])
             turn 0
             possible-words all-words]
        ;; (println "a" (peek todo))
        (let [[word path] (peek todo)
              new-path (conj path word)
              res [word path turn (count path) (count todo) (- all-words-count (count possible-words))]]
          ;; (println res)
          (cond
            (nil? word) res
            (= word end-word) (do (print-path word path) res)
            :else (let [new-words (make-fn possible-words word)
                        new-todo (for [w new-words] [w new-path])
                        t (into (pop todo) new-todo)]
                    ;; (println new-words)
                    ;; (println new-todo)
                    (recur t (inc turn) (difference possible-words new-words)))))))))

(defn find-path
  ([start-word end-word]
    (find-path start-word end-word lazy/make-words))
  ([start-word end-word make-fn]
    (let [all-words (load-words (count start-word))]
      (loop [seen #{}
             todo (conj empty-q [start-word []])
             turn 0]
        ;; (println "a" (peek todo))
        (let [[word path] (peek todo)
              new-path (conj path word)
              res [word path turn (count path) (count todo) (count seen)]]
          ;; (println res)
          (cond
            (nil? word) res
            (= word end-word) (do
                                ;(print-path word path)
                                res)
            :else (let [new-words (remove seen (make-fn all-words word))
                        new-todo (for [w new-words] [w new-path])
                        t (into (pop todo) new-todo)]
                    ;; (println new-words)
                    ;; (println new-todo)
                    (recur (into (conj seen word) new-words) t (inc turn)))))))))


;(quick-bench (find-path "fig" "dog"))
;(quick-bench (find-path-recur "fig" "dog"))
;(quick-bench (find-path "pearl" "water"))
;(quick-bench (find-path-recur "pearl" "water"))
;(quick-bench (find-path "fig" "xxx"))
;(quick-bench (find-path-recur "fig" "xxx"))
