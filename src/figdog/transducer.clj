(ns figdog.transducer
  (:use [clojure.set] [criterium.core] [figdog.core]))

(defn make-words-first [same-size-words word]
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
    (into #{} xform (range (count word))) #_result))

(defn words-seq-first
  ([word]
    (words-seq-first (conj '() [word nil]) (load-words-t (count word))))
  ([todo possible-words]
    (let [[word _ :as word-and-path] (first todo)
          words (make-words-first possible-words word)
          new-pairs (map vector words (repeat word-and-path))]
      (lazy-seq
        (cons word-and-path (words-seq-first (concat (rest todo) new-pairs)
                                             (difference possible-words words)))))))


(defn seen-once []
  (let [seen (volatile! #{})]
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
          (if (@seen input)
            (do
              ;(println "ignoring" input)
              result)
            (do
              ;(println "conj'ing" input seen)
              (vswap! seen conj input)
              (rf result input))))))))

;(into [] (seen-once) (concat (repeat 5 1) (repeat 3 3) (range 10)))


(defn make-words [same-size-words word seen-transducer]
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
                seen-transducer
                )]
    ;(println (range (count word)) xform)
    (eduction xform (range (count word))) #_result))

(defn words-seq
  ([word]
    (words-seq (conj '() [word nil]) (load-words-t (count word)) (seen-once)))
  ([todo possible-words seen-transducer]
    (let [[word _ :as word-and-path] (first todo)
          words (make-words possible-words word seen-transducer)
          new-pairs (map vector words (repeat word-and-path))]
      (lazy-seq
        (cons word-and-path (words-seq (concat (rest todo) new-pairs)
                                       possible-words seen-transducer))))))

(defn find-path
  ([start-word end-word]
    (find-path start-word end-word words-seq))
  ([start-word end-word seq-fn]
    ;(println "transducer find path" start-word end-word)
    (first (drop-while #(not= end-word (first (% 0)))
                       (map vector
                            (seq-fn start-word)
                            (iterate inc 0))))))

(println (find-path "fig" "dog" words-seq-first))
(println (find-path "fig" "dog" words-seq))

