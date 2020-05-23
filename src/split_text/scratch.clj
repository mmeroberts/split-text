(ns split-text.scratch
  (:require [clojure.string :as str]
            [com.rpl.specter :refer :all]))

(def j (line-seq(clojure.java.io/reader "C:\\Users\\MartinRoberts\\Dropbox (Personal)\\texts\\files\\11May2020\\Modified\\James23052020.md")))

(def l1 (nth j 0))
(def l10 (nth j 10))
(def blank "   ")

(def sd (drop 12 (take 44 j)))

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


(defn clean-markdown [md]
  (loop [ [ e & r :as all] md o []]
    (cond (and (or(nil? e)(empty? e)) (not (nil? r)) ) (recur r o)
          (and (nil? e) (nil? r)) o
          :else (let [header? (re-find header-regex e)
                      blank? (or (empty? e) (re-find blank-regex e))
                        text? (re-find text-regex e)]
                    (cond
                      header? (recur r (conj o e))
                      blank? (recur r (conj o e))
                      text? (let [d (handle-text all)
                                  o1 (first d)
                                  r1 (last d)]
                              (recur r1 (into  o (list o1))))
                      :else (recur r (conj o (str "else:" e))))))))


;(select [ALL FIRST] (re-seq #"((?:\d{1,3})\D+)" f))