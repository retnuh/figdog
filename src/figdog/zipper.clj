(ns figdog.zipper
  (:require [clojure.zip :as zip])
  (:use [clojure.set] [criterium.core] [figdog.core]))


;; TODO needs some testing, benchmarks, etc
;; haven't looked at this in years

(defn node-children [node] (or (:existing-children (meta node))
                               (let [all-words (:all-words (meta node))]
                                 (map #(with-meta (conj #{} %) (meta node))
                                      (make-words all-words (first node))))))

(defn word-zip
  "Returns a zipper for words, given a root word, where the children are the possible,
legal permutations"
  [root]
  (zip/zipper (fn [node] true)
              node-children
              (fn [node children] (with-meta node (assoc (meta node) :existing-children children)))
              (with-meta #{root} {:all-words (load-words (count root))})))


(defn siblings-seq
  [zipper]
  (lazy-seq
    (cons zipper (if-let [siblings (zip/right zipper)]
                   (siblings-seq siblings)
                   '()))))

(defn breadth-first-seq
  ([zipper]
    (breadth-first-seq (cons zipper '()) #{}))
  ;; todo: fix data hiding and get rid of unused arg
  ([todo seen]
    (let [zipper (first todo)
          all-children (if (zip/branch? zipper) (siblings-seq (zip/down zipper)))
          children (filter #(not (seen (zip/node %))) all-children)]
      (lazy-seq
        (cons zipper
              (breadth-first-seq (concat (rest todo) children)
                                 (into (conj seen (zip/node zipper)) (map zip/node children))))))))

;; (println "here")

(defn find-path-zipper [start-word end-word]
  (first (drop-while #(not= end-word (first (zip/node (% 0))))
                     (map vector
                          (breadth-first-seq (word-zip start-word))
                          (iterate inc 0)))))

(defn print-path-zipper [start-word end-word]
  (let [[path count] (time (find-path-zipper start-word end-word))]
    (print-path end-word (map first (zip/path path)))
    (println count " rounds")))

