(ns figdog.transducer
  (:use [clojure.set] [criterium.core] [figdog.core]))

(defn make-words [same-size-words word]
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

(defn words-seq
  ([word]
    (words-seq (conj '() [word nil]) (load-words-t (count word))))
  ([todo possible-words]
    (let [[word _ :as pair] (first todo)
          words (make-words possible-words word)
          new-pairs (map vector words (repeat pair))]
      (lazy-seq
        (cons pair (words-seq (concat (rest todo) new-pairs)
                              (difference possible-words words)))))))

(defn find-path [start-word end-word]
  (println "transducer find path" start-word end-word)
  (first (drop-while #(not= end-word (first (% 0)))
                     (map vector
                          (words-seq start-word)
                          (iterate inc 0)))))

(println (find-path "fig" "dog"))
