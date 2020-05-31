(ns split-text.io
  (:require [split-text.config :refer :all]
            [clojure.pprint :as pprint]
            [hickory.core :as h]
            [clojure.data.json :as json]
            [clojure.string :as str]

            [clojure.data.json :as json]))

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

(defn output-pprint [content output-filename]
  (spit output-filename (with-out-str (pprint/pprint content))))

(defn output-json [content output-filename]
  (spit output-filename (with-out-str (json/write-str content))))

(defn output-md [content output-filename]
  (spit output-filename (str row-normal-image " " (reduce str content) " " row-tiny-image)))
