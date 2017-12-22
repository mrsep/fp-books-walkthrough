(ns aoc-2017-clj.all
  (:gen-class)
  (:require [aoc-2017-clj.day1 :as day1]
            [aoc-2017-clj.day2 :as day2]
            [aoc-2017-clj.day3 :as day3]
            [aoc-2017-clj.day4 :as day4]
            [aoc-2017-clj.day5 :as day5]
            [aoc-2017-clj.day6 :as day6]
            [aoc-2017-clj.day7 :as day7]
            [aoc-2017-clj.day8 :as day8]
            ))

(defn -main [& args]
  (day1/-main)
  (day2/-main)
  (day3/-main)
  (day4/-main)
  (day5/-main)
  (day6/-main)
  (day7/-main)
  (day8/-main)
  )
