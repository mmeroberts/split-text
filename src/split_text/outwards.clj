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
  (vec(filter filter-bo-with-headings content)))

(defn get-contents [entry]
  (let [content (:content entry)]
    (if (vector? content) (str/join "" content)
                          content)))
(defn div [text]
  [:div text])

(defn cite [entry lang]
  [:span {:class (str/join "-" ["cite" (name lang)])} (get-contents entry)])

(defn h1 [entry lang]
  [:span {:class (str/join "-" ["h1" (name lang)])} (get-contents entry)])

(defn h2 [entry lang]
  [:span {:class (str/join "-" ["h2" (name lang)])} (get-contents entry)])

(defn u [entry lang]
  [:span {:class (str/join "-" ["u" (name lang)])} (get-contents entry)])



(defn p [entry lang]
  (let [text (get-contents entry)]
    (cond (= (:subtag entry) :u) (u entry lang)
          (= (:sub-type entry) :verse-number)  (list [:br][:span {:class "vn"} text])
          :else [:span {:class (str/join "-" ["v" (name lang)])} text])))





(defn render [content]
  (let [grouped (group-by (juxt :chapter :verse) content)]))

(defn get-chapter-and-verse-numbers [content]
  (sort(set(select [ALL FIRST] (select [ALL (collect (multi-path :chapter :vint :verse))] content)))))

(defn output-header [title]
  [:head
   [:title title]
   [:style (slurp "c:\\Users\\MartinRoberts\\private_projects\\split-text\\resources\\css\\main.css")]])
   ;(include-js "http://code.angularjs.org/1.2.3/angular.min.js")])



(defn output-html [title content]
  (let [hiccup-text(for [entry content]
                     (let [lang (:class entry)
                           tag (:tag entry)]
                       ((resolve (read-string (name tag))) entry lang)))
        hiccup-header (output-header title)]
    (h/html (list hiccup-header (into [:body ] hiccup-text)))))


(def header-regex #"#{1,3}.*")
(def blank-regex #"^\s$")
(def text-regex #"^[^#]*")

(defn split-verses [verstr]
  (select [ALL FIRST] (re-seq #"((?:\d{1,3})\D+)" verstr)))

(defn handle-text [t]
  "looks for lines of text that are deemed as text - they are not headings or blanks"
  (loop [[ex & rx :as allt] t ox ()]
    (cond (and (or(nil? ex)(empty? ex)) (not (nil? rx))) (recur rx ox)
          (and (nil? ex) (nil? rx))  (into (list (split-verses (str/join " " (flatten ox))) allt))
          :else (let [header1? (re-find header-regex ex)]
                  (if header1?
                    (into (list (split-verses (str/join " "(flatten ox))) allt))
                    (recur rx (cons ox (list ex))))))))






;(h/html(into [:div] (output-html (vec(output-bo-book-content doc)))))

