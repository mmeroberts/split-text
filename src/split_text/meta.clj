(ns split-text.meta
  (:require [split-text.config :refer :all]
            [split-text.crux :refer :all]
            [com.rpl.specter :refer :all]
            [clojure.string :as str]))

(defn get-chapter-numbers [db book]
  (let [q (query-chapter-numbers db book)
        ch-set (sort(flatten (seq q)))]
    (assoc {} :chapters ch-set :last-chaper (last ch-set))))

(defn get-a-chapters-verse-numbers [db book chapter]
  (let [q (query-verse-numbers-by-chapter db book chapter)
        q-set (set q)]))

(defn handle-verse-numbers [q book]
  (let [vs-set (set (sort (vec q)))]
    (group-by :chapter
      (for [s vs-set]
        (let [ch (first s)
              vs (last s)
              fv (if (str/includes? vs "-") (first (str/split vs #"-")) vs)
              ivs (Integer. fv)]
          (assoc {} :book book :chapter ch :verse-str vs :verse-number ivs))))))

(defn get-verse-numbers [db book]
  (let [q (query-verse-numbers db book)]
    (handle-verse-numbers q book)))

(defn get-verse-numbers-by-language [db book language]
  (let [q (query-verse-numbers-by-language db book language)]
    (handle-verse-numbers q book)))



(defn build-verse-metadata [verses book]
  (for [k (keys verses)]
    (let [maxvs (apply max-key #(:verse-number %) (get verses k))
          minvs (apply min-key #(:verse-number %) (get verses k))
          maxv (:verse-number maxvs)
          minv (:verse-number minvs)]

      (assoc {} :crux.db/id (keyword  book (str"Meta-" k))
                :book book
                :chapter k
                :min-verse-number minv
                :max-verse-number maxv))))

(defn get-book-chapter-metadata [db book]
  (let [verses (get-verse-numbers db book)]
    (build-verse-metadata verses book)))

(defn get-book-chapter-metadata-by-language [db book language]
  (let [verses (get-verse-numbers-by-language db book language)]
    (build-verse-metadata verses book)))

;(sort (select [ALL FIRST] (select [ 2 ALL (collect (multi-path :verse-number :verse-str  ) )] (get-verse-numbers cruxdb "James"))))



