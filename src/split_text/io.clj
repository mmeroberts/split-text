(ns split-text.io
  (:require [clojure.pprint :as pprint]
            [hickory.core :as h]
            [clojure.string :as str]))

(defn clean [text]
  (-> text
      (str/replace "\n" " ")
      (str/replace "\r" " ")
      (str/replace "\ufffd" "")
      (str/replace "\u00a0" " ")
      (str/trim-newline)))

(defn as-hickory-read-file [filename]
  (let [raw (clean (slurp filename))
        parsed-doc (h/parse raw)]
    (h/as-hickory parsed-doc)))

(defn output-content [content output-filename]
  (spit output-filename (with-out-str (pprint/pprint content))))
