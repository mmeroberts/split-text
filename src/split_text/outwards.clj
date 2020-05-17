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
