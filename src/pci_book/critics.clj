(ns pci-book.critics
  (:require [clojure.set :as clset]
            [clojure.tools.logging :as log]
            [clojure.walk :as walk]
            [criterium.core :as bench]))

(def critics {"Lisa Rose"  {"Lady in the Water"  2.5  "Snakes on a Plane"  3.5
                            "Just My Luck"  3.0  "Superman Returns"  3.5  "You Me and Dupree"  2.5
                            "The Night Listener"  3.0}
              "Gene Seymour"  {"Lady in the Water"  3.0  "Snakes on a Plane"  3.5
                               "Just My Luck"  1.5  "Superman Returns"  5.0  "The Night Listener"  3.0
                               "You Me and Dupree"  3.5}
              "Michael Phillips"  {"Lady in the Water"  2.5  "Snakes on a Plane"  3.0
                                   "Superman Returns"  3.5  "The Night Listener"  4.0}
              "Claudia Puig"  {"Snakes on a Plane"  3.5  "Just My Luck"  3.0
                               "The Night Listener"  4.5  "Superman Returns"  4.0
                               "You Me and Dupree"  2.5}
              "Mick LaSalle"  {"Lady in the Water"  3.0  "Snakes on a Plane"  4.0
                               "Just My Luck"  2.0  "Superman Returns"  3.0  "The Night Listener"  3.0
                               "You Me and Dupree"  2.0}
              "Jack Matthews"  {"Lady in the Water"  3.0  "Snakes on a Plane"  4.0
                                "The Night Listener"  3.0  "Superman Returns"  5.0  "You Me and Dupree"  3.5}
              "Toby"  {"Snakes on a Plane" 4.5 "You Me and Dupree" 1.0 "Superman Returns" 4.0}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; euclidian distance score
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sim-distance [prefs person1 person2]
  (let [prefs-p1       (prefs person1)
        prefs-p2       (prefs person2)
        shared-items   (clset/intersection (set (keys prefs-p1)) (set (keys prefs-p2)))]
    (when (not-empty shared-items)
      (->> (map #(Math/pow (- (prefs-p1 %)
                              (prefs-p2 %))
                           2)
                shared-items)
           (reduce +)
           (Math/sqrt)
           (+ 1)
           (/ 1)
           ))))

(defn sim-distance1 [prefs person1 person2]
  (->> (for [[m1 r1] (prefs person1)]
         (first (for [[m2 r2] (prefs person2)
                      :when (= m1 m2)]
                  (Math/pow (- r1 r2)
                            2))))
       (reduce +)
       (Math/sqrt)
       (+ 1)
       (/ 1)
       ))

(do
  (print "for: ")
  (time (sim-distance1 critics "Lisa Rose" "Gene Seymour"))
  (print "map: ")
  (time (sim-distance critics "Lisa Rose" "Gene Seymour")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pearson Correlation Score
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sim-pearson [prefs person1 person2]
  (let [prefs-p1       (prefs person1)
        prefs-p2       (prefs person2)
        shared-items   (clset/intersection (set (keys prefs-p1)) (set (keys prefs-p2)))
        n              (count shared-items)]
    (if (not-empty shared-items)
      (let [sum1   (reduce + (map #(prefs-p1 %) shared-items))
            sum2   (reduce + (map #(prefs-p2 %) shared-items))
            sum1Sq (reduce + (map #(Math/pow (prefs-p1 %) 2) shared-items))
            sum2Sq (reduce + (map #(Math/pow (prefs-p2 %) 2) shared-items))
            pSum   (reduce + (map #(* (prefs-p1 %) (prefs-p2 %)) shared-items))
            numm   (- pSum (* sum1 (/ sum2 n)))
            den    (Math/sqrt (* (- sum1Sq (/ (Math/pow sum1 2) n))
                                 (- sum2Sq (/ (Math/pow sum2 2) n))))]
        (if (== den 0)
          0
          (/ numm den)))
      0)))

;; pokusaj optimizacije
(defn sim-pearson1 [prefs person1 person2]
  (let [prefs-p1       (prefs person1)
        prefs-p2       (prefs person2)
        shared-items   (clset/intersection (set (keys prefs-p1)) (set (keys prefs-p2)))
        n              (count shared-items)]
    (if (not-empty shared-items)
      (let [sum1   (reduce #(+ %1 (prefs-p1 %2)) 0 shared-items)
            sum2   (reduce #(+ %1 (prefs-p2 %2)) 0 shared-items)
            sum1Sq (reduce #(+ %1 (Math/pow (prefs-p1 %2) 2)) 0 shared-items)
            sum2Sq (reduce #(+ %1 (Math/pow (prefs-p2 %2) 2)) 0 shared-items)
            pSum   (reduce + (map #(* (prefs-p1 %) (prefs-p2 %)) shared-items))
            numm   (- pSum (* sum1 (/ sum2 n)))
            den    (Math/sqrt (* (- sum1Sq (/ (Math/pow sum1 2) n))
                                 (- sum2Sq (/ (Math/pow sum2 2) n))))]
        (if (== den 0)
          0
          (/ numm den)))
      0)))


(do
  (time (sim-pearson critics "Lisa Rose" "Gene Seymour"))

  (time (sim-pearson1 critics "Lisa Rose" "Gene Seymour")))

; (reduce #(+ %1 (prefs-p1 %2)) 0 shared-items)

;;;;;;;;;;;;;;;;;;;;;;
;; Ranking the Critics
;;;;;;;;;;;;;;;;;;;;;;

(defn top-matches
  "return best matches for person, default similarity function = Pearson Correlation Score"
  ([prefs person]
   (let [others       (dissoc prefs person)
         sim (partial sim-pearson prefs person)
         my-map (->> (filter #(> (sim (key %)) 0)
                             others)
                     keys
                     (reduce #(assoc %1 %2 (sim %2)) {}))]
     (into (sorted-map-by (fn [key1 key2] (compare (get my-map key2) (get my-map key1)))) my-map)))
  ;([prefs person n similarity]
   ;(->> (map #(conj [%] (sim-pearson prefs person %)) (remove #{person} (keys prefs)))
    ;    (sort-by #(second %))
     ;   reverse
      ;  (take n)))
  )

(top-matches critics "Toby")


;;;;;;;;;;;;;;;;;;;;;
;; Recommending Items
;;;;;;;;;;;;;;;;;;;;;

(defn remove-all[from in]
  (remove (fn [[k v]] ((apply hash-set (keys from)) k)) in))

(defn get-movie-critic [m c]
  (->>(reduce #(assoc %1 (key %2) ((val %2) m))
              {} c)
      (remove #(nil? (val %)))
      (into {})))

(get-movie-critic "Just My Luck" critics)

;;;;;;;;;;;;;;;;;;;;
;; old implentation
;;;;;;;;;;;;;;;;;;;;

#_(defn get-recommendations [prefs person similarity]
  (let [others        (dissoc prefs person)
        similar       (top-matches prefs person)
        unseen-movies (reduce #(->>
                                (remove-all (prefs person) (val %2))
                                (into {})
                                (assoc %1 (key %2)))
                              {}
                              others)

        unseen-movie-names (reduce #(apply conj %1 (keys (val %2))) #{} unseen-movies)

        rankings (reduce #(assoc %1 %2 (get-movie-critic %2 others)) {} unseen-movie-names)

        mult-rank (reduce #(assoc %1 (key %2) (reduce
                                               (fn [x y]
                                                 (assoc x (key y) (* (val y)
                                                                     (similarity prefs person (key y)))))
                                               {}
                                                 (filter (fn [[k v]] (if (>(*(similarity prefs person k)
                                                                             v)
                                                                           0)
                                                                       true))
                                                         (val %2))))
                          {} rankings)

        totals (reduce #(assoc %1 (key %2)
                          (->
                           (reduce + (vals(val %2)))
                           (/ (reduce + (->>
                                         (filter (fn [x] (if (contains? (val %2) (key x))
                                                                          true)
                                                                     ) similar)
                                         (into {})
                                         vals))
                              )
                           ))
                       {} mult-rank)]
    totals))





(defn get-unseen-movies
  "Gets unseen movies for person. Returns ({\"Movie\" {:sim similarity :rating (* rating similarity)}, ..."
  [prefs person similarity]
  (->> (for [[other crtcs] (dissoc prefs person)
             :let [sim (similarity prefs person other)]
             :when (> sim 0)]
         (for [[m r] crtcs
                        :when (not (contains? (set (keys (prefs person))) m))]
                    {m {:sim sim :rating (* sim r)}}))
       (apply concat)))

;(get-unseen-movies critics "Toby" sim-pearson)

(defn get-recommendations
  [prefs person similarity]
  (let [x (reduce (fn [accum x]
                    (let [k (apply key x)
                          v (apply val x)]
                      (if (empty? (accum k))
                        (assoc accum k {:sim-sum (:sim v)
                                        :ratings [(:rating v)]})
                        (assoc accum k {:sim-sum (+ (:sim-sum (accum k))
                                                    (:sim v))
                                        :ratings (conj (:ratings (accum k))
                                                       (:rating v))}))))
                  {}
                  (get-unseen-movies prefs person similarity))]
    (into
     {}
     (for [[k v] x
           :let [r (:ratings v)
                 total (reduce + r)
                 total-sim-sum (/ total (:sim-sum v))]]
       [k total-sim-sum]))))


(do
  (time (get-recommendations critics "Toby" sim-pearson))
  (time (get-recommendations critics "Toby" sim-pearson1)))


;; (bench/with-progress-reporting(bench/bench (get-recommendations critics "Toby" sim-pearson)))
