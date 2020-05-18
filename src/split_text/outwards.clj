(ns split-text.outwards
  (:require [split-text.config :refer :all]
          [split-text.db :refer :all]
          [split-text.io :refer :all]
          [split-text.crux :refer :all]
          [split-text.meta :refer :all]
          [crux.api :as crux]
          [com.rpl.specter :refer :all]
          [hickory.select :as s]
          [clojure.string :as str]
          [clojure.data.json :as json]))

(defn filter-bo-with-headings [x]
  (let [tag (:tag x)
        class (:class x)]
    (cond (and (contains? #{:cite :h1} tag) (not= class :blank)) true
          (and (= tag :h2) (= class :bo)) true
          (and (= tag :p) (= class :bo)) true
          :else false)))

(defn filter-eng-with-headings [x]
  (let [tag (:tag x)
        class (:class x)]
    (cond (and (contains? #{:cite :h1} tag) (not (contains? #{:bo :back :blank} class))) true
          (and (= tag :h2) (= class :eng)) true
          (and (= tag :p) (= class :eng)) true
          :else false)))

(defn filter-back-with-headings [x]
  (let [tag (:tag x)
        class (:class x)]
    (cond (and (contains? #{:cite :h1 :h2} tag) (not (contains? #{:bo :blank} class))) true
          (and (= tag :p) (= class :back)) true
          :else false)))

(defn output-verse-by-language-doc [db book chapter verse language]
  (let [v (query-text-by-chapter-and-verse db book chapter verse language)
        text (for [[index subindex tag subtag content]  v]
               (let [verse-number? (not (nil? (re-find #"^(?: )?[-0-9]+" content)))
                     name? (if (= subtag :u) true false)]
                 (cond verse-number? (str content " ")
                       name? (str "<u>" content "</u>")
                       :else content)))]
    (str "<p>" (str/join "" text) "</p>")))

(defn output-verse-by-language [db book chapter verse language]
  (let [v (query-text-by-chapter-and-verse db book chapter verse language)
        text (for [[index subindex tag subtag content]  v]
               (let [verse-number? (not (nil? (re-find #"^(?: )?[-0-9]+" content)))
                     name? (if (= subtag :u) true false)]
                 (cond verse-number? (str content " ")
                       name? (str "<u>" content "</u>")
                       :else content)))]
    (str "<p>" (str/join "" text) "</p>")))

(defn output-book-by-langage [db book language]
  (let [book-meta (get-verse-numbers-by-language db book language)
        chapters (sort(keys book-meta))]
    (for [chapter chapters]
      (let [verse-meta (sort-by (juxt :chapter :verse-number) (get book-meta chapter))
            text (for [vm verse-meta]
                   (output-verse-by-language db book chapter (:verse-str vm) language))]
        (assoc {} :chapter chapter :text text)))))





