(ns figdog.transducer
  (:use [clojure.set] [criterium.core] [figdog.core])
  (:import (java.util HashSet)))

(defn snoop [name]
  (fn [rf]
    (fn
      ([]
        (let [r (rf)]
          (println "snoop" name "init" r)
          r))
      ([result]
        (println "snoop" name "complete")
        (rf result))
      ([result input]
        (println "snoop" name "input" input)
        (rf result input))
      )))


(defn make-words-first [same-size-words word]
  (let [words-at-index (fn [index]
                         (let [pre (.substring word 0 index)
                               c (.charAt word index)
                               post (.substring word (inc index))]
                           ;(println "whee" word index pre post c)
                           (into [] (comp
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
    (into [] xform (range (count word))) #_result))


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


(deftype Delayed [val]
  Iterable
  (iterator [_] (.iterator ^java.util.Collection @val))

  clojure.lang.Seqable
  (seq [_] (seq @val))

  clojure.lang.IReduceInit
  (reduce [_ f init]
    (reduce f init @val))

  clojure.lang.Counted
  (count [_] (count @val))

  clojure.lang.Sequential)

(defn delayed
  "Returns a reducible/iterable/seqable application of
  the transducer to the items in coll. Note that these applications
  will be performed every time reduce/iterator/seq is called."
  {:added "1.7"}
  [start xform coll]
  (Delayed. (delay (into start xform coll))))

(defn seen-once []
  ; Note that I am intentionally "breaking" the contract for a stateful transducer -
  ; I want to keep state across multiple bindings to reduction functions
  (let [seen (HashSet.)]
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
          (if (.contains seen input)
            (do
              ;(println "ignoring" input)
              result)
            (do
              ;(println "conj'ing" input seen)
              (.add seen input)
              (rf result input))))))))

;(into [] (seen-once) (concat (repeat 5 1) (repeat 3 3) (range 10)))


(defn make-words [same-size-words word seen-transducer]
  (let [words-at-index (fn [index]
                         (let [pre (.substring word 0 index)
                               c (.charAt word index)
                               post (.substring word (inc index))]
                           ;(println "whee" word index pre post c)
                           (delayed [] (comp
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
    (delayed [] xform (range (count word))) #_result))


(defn words-seq
  ([word]
    (words-seq (conj '() [word nil]) (load-words-t (count word)) (seen-once)))
  ([todo possible-words seen-transducer]
    (let [[word _ :as word-and-path] (first todo)
          words (make-words possible-words word seen-transducer)
          new-pairs (map vector words (repeat word-and-path))]
      ;(println "words" words)
      (lazy-seq
        (cons word-and-path (words-seq (concat (rest todo) new-pairs)
                                       possible-words seen-transducer))))))

(defn find-path
  ([start-word end-word]
    (find-path start-word end-word words-seq))
  ([start-word end-word seq-fn]
    ;(println "transducer find path" start-word end-word)
    (first
      (sequence
        (comp
          (map-indexed vector)
          (drop-while #(not= end-word (first (% 1))))
          )
        (seq-fn start-word)))))

;(println (time (find-path "fig" "dog" words-seq-first)))
;(println (time (find-path "fig" "dog" words-seq)))
;(println (time (find-path "pearl" "water" words-seq-first)))
;(println (time (find-path "pearl" "water" words-seq)))
;(quick-bench (find-path "pearl" "water" words-seq-first))
;(quick-bench (find-path "pearl" "water" words-seq))
;
;(bench (find-path "pearl" "water" words-seq-first))
;(bench (find-path "pearl" "water" words-seq))

;(sequence (comp
;           (snoop "first")
;           (mapcat #(repeat % %))
;           (snoop "map")
;           ;(filter even?)
;           ;(snoop "filter")
;           )
;      (range 5))
;(into [] (seen-once) (concat (repeat 5 1) (repeat 3 3) (range 10)))
