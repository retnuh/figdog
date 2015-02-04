(ns figdog.bench
  (:use [criterium.core])
  (:require
    [figdog.core :as core]
    [figdog.lazy :as lazy]
    [figdog.transducer :as trans]
    [figdog.recursive :as recursive]))

(quick-bench lazy/find-path "fig" "dog")
(lazy/find-path "fig" "dog")
(quick-bench trans/find-path "fig" "dog")
(trans/find-path "fig" "dog")

(quick-bench lazy/find-path "pearl" "water")
(lazy/find-path "pearl" "water")
(quick-bench trans/find-path "pearl" "water")
(trans/find-path "pearl" "water")

