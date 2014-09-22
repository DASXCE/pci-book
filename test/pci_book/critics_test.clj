(ns pci-book.critics-test
  (:use midje.sweet)
  (:require [pci-book.critics :refer :all]))


(facts "about smth"
       (fact "it is ok"
             (first [1 2 3 5]) => 1)
       (fact "should fail"
             (first [1 2 3 5]) =not=> 1))
