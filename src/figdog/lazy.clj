(ns figdog.lazy
  (:use [clojure.set] [criterium.core] [figdog.core]))

(defn make-words [actual-words word]
  (let [possibles (for [index (range (count word))
                        :let [pre (.substring word 0 index)
                              c (.charAt word index)
                              post (.substring word (inc index))]
                        other (range (int \a) (inc (int \z))) :when (not (== other (int c)))]
                    (str pre (char other) post))]
    ;; (println possibles)
    (filter actual-words possibles)))

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

(defn find-path [start-word end-word]
  ;(println "lazy find path" start-word end-word)
  (first (drop-while #(not= end-word (first (% 1)))
                     (map-indexed vector (words-seq start-word)))))


(println (find-path "fig" "dog"))
