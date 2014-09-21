(ns pci-book.core-test
  (:require [clojure.test :refer :all]
            [pci-book.critics :refer :all]))

(("Lisa Rose"
            {"Just My Luck" 2.97372212148579} {"Lady in the Water" 2.4781017679048247} {"The Night Listener" 2.97372212148579})
 ("Gene Seymour"
            {"Just My Luck" 0.5718696387472675} {"Lady in the Water" 1.143739277494535} {"The Night Listener" 1.143739277494535})
 ("Michael Phillips"
            {"Lady in the Water" -2.5} {"The Night Listener" -4.0})
 ("Claudia Puig"
            {"Just My Luck" 2.680215442324694} {"The Night Listener" 4.020323163487041})
 ("Mick LaSalle"
            {"Just My Luck" 1.8489469032838097} {"Lady in the Water" 2.7734203549257144} {"The Night Listener" 2.7734203549257144})
 ("Jack Matthews"
            {"Lady in the Water" 1.9885469410796102} {"The Night Listener" 1.9885469410796102}))


(map #(vec %) {:a 1 :b 2 :c 3})
