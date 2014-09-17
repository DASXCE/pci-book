(ns pci-book.critics
  (:require [clojure.set :as clset]
            [clojure.tools.logging :as log]))

(def critics {"Lisa Rose"  {"Lady in the Water"  2.5  "Snakes on a Plane"  3.5
                            "Just My Luck"  3.0  "Superman Returns"  3.5  "You  Me and Dupree"  2.5
                            "The Night Listener"  3.0}
              "Gene Seymour"  {"Lady in the Water"  3.0  "Snakes on a Plane"  3.5
                               "Just My Luck"  1.5  "Superman Returns"  5.0  "The Night Listener"  3.0
                               "You  Me and Dupree"  3.5}
              "Michael Phillips"  {"Lady in the Water"  2.5  "Snakes on a Plane"  3.0
                                   "Superman Returns"  3.5  "The Night Listener"  4.0}
              "Claudia Puig"  {"Snakes on a Plane"  3.5  "Just My Luck"  3.0
                               "The Night Listener"  4.5  "Superman Returns"  4.0
                               "You  Me and Dupree"  2.5}
              "Mick LaSalle"  {"Lady in the Water"  3.0  "Snakes on a Plane"  4.0
                               "Just My Luck"  2.0  "Superman Returns"  3.0  "The Night Listener"  3.0
                               "You  Me and Dupree"  2.0}
              "Jack Matthews"  {"Lady in the Water"  3.0  "Snakes on a Plane"  4.0
                                "The Night Listener"  3.0  "Superman Returns"  5.0  "You  Me and Dupree"  3.5}
              "Toby"  {"Snakes on a Plane" 4.5 "You  Me and Dupree" 1.0 "Superman Returns" 4.0}})

;; euclidian distance score

(defn sim-distance [prefs person1 person2]
  (let [prefs-p1       (prefs person1)
        prefs-p2       (prefs person2)
        shared-items   (clset/intersection (set (keys prefs-p1)) (set (keys prefs-p2)))
        sum-of-squares 0]
    (when (not-empty shared-items)
      (->> (reduce + (map (fn [i]
                            (Math/pow (- (prefs-p1 i)
                                         (prefs-p2 i))
                                      2))
                          shared-items))
           (Math/sqrt)
           (+ 1)
           (/ 1)))))


(sim-distance critics "Lisa Rose" "Gene Seymour")


;; Pearson Correlation Score

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


(sim-pearson critics "Lisa Rose" "Gene Seymour")


(defn top-matches
  "return best matches for person, default number of results = 3, default similarity function = Pearson Correlation Score"
  ([prefs person]
   (->> (map #(conj [%] (sim-pearson prefs person %)) (remove #{person} (keys prefs)))
        (sort-by #(second %))
        reverse
        ;(take 3)
        ))
  ([prefs person n similarity]
   (->> (map #(conj [%] (sim-pearson prefs person %)) (remove #{person} (keys prefs)))
        (sort-by #(second %))
        reverse
        (take n))))


(top-matches critics "Toby")

(defn get-recommendations [prefs person similarity]
  (let [others (dissoc prefs person)
        unseen-movies (map #(->> (conj (filter (fn [c] (not (contains? (prefs person) (first c))))
                                               (prefs %))
                                       %)
                                 rest
                                 (reduce concat)
                                 (apply hash-map)
                                 (assoc {} %))
                           (keys (dissoc prefs person)))

        unseen-rated (map #(conj (map (fn [x] (assoc {} (key x) (* (similarity prefs person (apply key %))
                                                                   (val x))))
                                      (apply val %))
                                 (apply key %))
                          unseen-movies)

        sim-sum   (reduce +(map #(similarity prefs person (key %))
                                others))]
    ;unseen-rated
    sim-sum))

(map #(sim-pearson critics "Toby" ))

(map #(assoc {} (key %) (sim-pearson critics "Toby" (key %))) critics)
(reduce #(+ (apply val %1) (apply val %2)) '({:a 1} {:b 2}))

(get-recommendations critics "Toby" sim-pearson)

(def u-mm '({"Mick LaSalle" {"Just My Luck" 2.0, "Lady in the Water" 3.0, "The Night Listener" 3.0}} {"Claudia Puig" {"Just My Luck" 3.0, "The Night Listener" 4.5}} {"Lisa Rose" {"Just My Luck" 3.0, "Lady in the Water" 2.5, "The Night Listener" 3.0}} {"Jack Matthews" {"Lady in the Water" 3.0, "The Night Listener" 3.0}} {"Michael Phillips" {"Lady in the Water" 2.5, "The Night Listener" 4.0}} {"Gene Seymour" {"Just My Luck" 1.5, "Lady in the Water" 3.0, "The Night Listener" 3.0}}))

(map
 #(map (fn [x] (str "::::" (key x) ":::::"))
       (apply val %))
 u-mm)

(map #(u-mm (apply key %))
     u-mm)

(u-mm {"Mick LaSalle" {}})

(apply key {"Mick LaSalle" {"Just My Luck" 2.0, "Lady in the Water" 3.0, "The Night Listener" 3.0}})

