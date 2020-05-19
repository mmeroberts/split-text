(ns split-text.outwards
  (:require [split-text.config :refer :all]
          [split-text.db :refer :all]
          [split-text.io :refer :all]
          [split-text.crux :refer :all]
          [split-text.meta :refer :all]
          [crux.api :as crux]
          [com.rpl.specter :refer :all]
          [hickory.render :as hr]
          [clojure.string :as str]
          [clojure.data.json :as json]
          [hiccup2.core :as h]))

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


(defn output-bo-book-content [content]
  (filter filter-bo-with-headings content))

(defn cite [text lang]
  [:p {:class (str/join "-" ["cite" (name lang)])} text])

(defn h1 [text lang]
  [:h1 {:class (str/join "-" ["h1" (name lang)])} text])

(defn h2 [text lang]
  [:h2 {:class (str/join "-" ["h2" (name lang)])} text])

(defn u [text lang]
  [:span {:class (str/join "-" ["u" (name lang)])} text])

(defn div [text]
  [:div text])

(defn verse [text lang]
  (let [[n verse](rest(re-find #"^(?: *)([-0-9]+)(.*)" text))]
    [:p
     [:span {:class (str/join "-" ["vn" (name lang)]) } n]
     [:span {:class (str/join "-" [ "v" (name lang)])} verse]]))

(defn render [content]
  (let [grouped (group-by (juxt :chapter :verse) content)]))

(defn get-chapter-and-verse-numbers [content]
  (sort(set(select [ALL FIRST] (select [ALL (collect (multi-path :chapter :vint :verse))] content)))))

(defn get-contents [entry]
  (let [content (:content entry)]
    (if (vector? content) (str/join "" content)
                          content)))

(defn output-html [content]
  (let [working-content content
        hiccup-content(loop [i 0 holding-lang :eng holding "" output []]
                        (let [entry (get working-content i)
                              lang (:class entry)
                              tag (:tag entry)
                              text (if (and (contains? #{:cite :h1 :h2} tag))
                                     ((resolve (read-string (name tag))) (get-contents entry) lang))
                              this-output (if (or (not (nil? text)) (not= holding-lang lang))
                                            (if (empty? holding)
                                              text
                                              (conj (verse holding holding-lang) text)))
                              check-holding (if (not (nil? text)) "" holding)
                              this-holding-lang (if (= (:class entry) :blank) holding-lang lang)
                              this-holding (str check-holding (if (and (= tag :p) (not= (:class entry) :blank))
                                                                (if (= (:subtag entry) :u) (u (get-contents entry) lang)
                                                                                           (get-contents entry))))]
                          (if (> i (count working-content))
                            output
                            (if (= lang :blank)
                              (recur (inc i) holding-lang holding output)
                              (recur (inc i) this-holding-lang this-holding (if (not (nil? this-output)) (conj output this-output) output))))))]
    hiccup-content))





;(h/html(into [:div] (output-html (vec(output-bo-book-content doc)))))
