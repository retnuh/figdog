(ns figdog
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

(defn make-words [same-size-words word]
  (let [possibles (for [index (range (count word))
                        :let [pre (.substring word 0 index)
                              c (.charAt word index)
                              post (.substring word (inc index))]
                        other (range (int \a) (inc (int \z))) :when (not (== other (int c)))]
                    (str pre (char other) post))]
    ;; (println possibles)
    (filter same-size-words possibles)))

(defn make-words-t [same-size-words word]
  (let [words-at-index (fn [index]
                         (let [pre (.substring word 0 index)
                               c (.charAt word index)
                               post (.substring word (inc index))]
                           ;(println "whee" word index pre post c)
                           (eduction (comp
                                       ;(map #(do (println "wai1" word index pre c post (char %)) %))
                                       (filter #(not (== % (int c))))
                                       (map #(str pre (char %) post))
                                       ;(map #(do (println "wai2" %) %))
                                       )
                                     (range (int \a) (inc (int \z))))))
        xform (comp
                ;(map #(do (println "c1" %) %))
                (mapcat words-at-index)
                ;(map #(do (println "c2" %) %))
                (filter same-size-words)
                ;(map #(do (println "c3" %) %))
                ;#(into [] identity %)
                ;(map #(do (println "c3" % (into [] identity %)) %))
                )]
    ;(println (range (count word)) xform)
    (into #{} xform (range (count word))) #_result ))


;(make-words #{"fig" "foo"} "fog")

(defn words-seq
  ([word]
    (words-seq (conj '() [word nil]) (load-words (count word))))
  ([todo possible-words]
    (let [[word _ :as pair] (first todo)
          words (make-words possible-words word)
          new-pairs (map vector words (repeat pair))]
      (lazy-seq
        (cons pair (words-seq (concat (rest todo) new-pairs)
                              (difference possible-words words)))))))

(defn find-path-lazy [start-word end-word]
  (first (drop-while #(not= end-word (first (% 0)))
                     (map vector
                          (words-seq start-word)
                          (iterate inc 0)))))

(defn words-seq-t
  ([word]
    (words-seq-t (conj '() [word nil]) (load-words-t (count word))))
  ([todo possible-words]
    (let [[word _ :as pair] (first todo)
          words (make-words-t possible-words word)
          new-pairs (map vector words (repeat pair))]
      (lazy-seq
        (cons pair (words-seq (concat (rest todo) new-pairs)
                              (difference possible-words words)))))))

(defn find-path-t [start-word end-word]
  (first (drop-while #(not= end-word (first (% 0)))
                     (map vector
                          (words-seq-t start-word)
                          (iterate inc 0)))))

(defn print-path [word path]
  (doall (for [w path]
           (print w "-> ")))
  (and word (println word)))

(def empty-q PersistentQueue/EMPTY)

(defn print-path-lazy [start-word end-word]
  (let [[path count] (time (find-path-lazy start-word end-word))
        pp (fn [v])]
    (print-path end-word (map first (zip/path path)))
    (println count " rounds")))

;;;

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
              (with-meta  #{root} {:all-words (load-words (count root))})))


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

(defn find-path-recur [start-word end-word]
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
          :else (let [new-words (make-words possible-words word)
                      new-todo (for [w new-words] [w new-path])
                      t (into (pop todo) new-todo)]
                  ;; (println new-words)
                  ;; (println new-todo)
                  (recur t (inc turn) (difference possible-words new-words))))))))

(defn find-path [start-word end-word]
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
          (= word end-word) (do (print-path word path) res)
          :else (let [new-words (filter #(not (seen %)) (make-words all-words word))
                      new-todo (for [w new-words] [w new-path])
                      t (into (pop todo) new-todo)]
                  ;; (println new-words)
                  ;; (println new-todo)
                  (recur (into (conj seen word) new-words) t (inc turn))))))))

(println "there")
;
;(println (time (find-path "fig" "dog")))
;(print-path-zipper "fig" "dog")
;(println (time (find-path "fig" "xxx")))
;(println (time (find-path "pearl" "cream")))
;(println (time (find-path-lazy "pearl" "cream")))
;(print-path-zipper "pearl" "cream")
;(println (time (find-path "pearl" "water")))
;(print-path-zipper "pearl" "water")
(quick-bench (load-words 5))
(quick-bench (load-words-t 5))
