(ns figdog.bench
  (:use [criterium.core])
  (:require
    [figdog.core :as core]
    [figdog.lazy :as lazy]
    [figdog.transducer :as trans]
    [figdog.recursive :as recursive]))

(println "lazy" (lazy/find-path "fig" "dog"))
(quick-bench lazy/find-path "fig" "dog")
(println "trans first" (trans/find-path "fig" "dog" trans/words-seq-first))
(quick-bench (trans/find-path "fig" "dog" trans/words-seq-first))
(println "trans" (trans/find-path "fig" "dog" trans/words-seq))
(quick-bench (trans/find-path "fig" "dog" trans/words-seq))


(println "lazy " (lazy/find-path "pearl" "water"))
(quick-bench lazy/find-path "pearl" "water")
(println "trans first" (trans/find-path "pearl" "water" trans/words-seq-first))
(quick-bench (trans/find-path "pearl" "water" trans/words-seq-first))
(println "trans" (trans/find-path "pearl" "water" trans/words-seq))
(quick-bench (trans/find-path "pearl" "water" trans/words-seq))

; recursive driver

(println "recursive, lazy" (recursive/find-path "fig" "dog" lazy/make-words))
(quick-bench (recursive/find-path "fig" "dog" lazy/make-words))
(println "recursive, trans first" (recursive/find-path "fig" "dog" trans/make-words-first))
(quick-bench (recursive/find-path "fig" "dog" trans/make-words-first))
(println "recursive, trans" (recursive/find-path "fig" "dog"
                                                 (let [seen (trans/seen-once)]
                                                   #(trans/make-words %1 %2 seen))))
(quick-bench (recursive/find-path "fig" "dog"
                                  (let [seen (trans/seen-once)]
                                    #(trans/make-words %1 %2 seen))))

(println "recursive, lazy" (recursive/find-path "pearl" "water" lazy/make-words))
(quick-bench (recursive/find-path "pearl" "water" lazy/make-words))
(println "recursive, trans first" (recursive/find-path "pearl" "water" trans/make-words-first))
(quick-bench (recursive/find-path "pearl" "water" trans/make-words-first))
(println "recursive, trans" (recursive/find-path "pearl" "water"
                                                 (let [seen (trans/seen-once)]
                                                   #(trans/make-words %1 %2 seen))))
(quick-bench (recursive/find-path "pearl" "water"
                                  (let [seen (trans/seen-once)]
                                    #(trans/make-words %1 %2 seen))))
