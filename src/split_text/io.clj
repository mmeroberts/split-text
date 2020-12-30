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

(defn filter-chapter-bo-headings [l]
  ;(or
  (and (= (:lang l) :bo) (contains? #{ :h2 } (:type l))))  ;(= (:type l) :h1)))

(defn create-chapter-links [md]
  (let [chapters (filter filter-chapter-bo-headings md)]
    (for [c chapters]
      (str "<a href=\"#" (str/trim (:text c)) "\">" (str/trim(:text c)) "</a>"))))


(defn make-menu-div [md]
  (str
    "<div id=\"mySidenav\" class=\"sidenav\">"
    "<a href=\"javascript:void(0)\" class=\"closebtn\" onclick=\"closeNav()\">&times;</a>"
    "<a href=\"javascript:void(0)\" class=\"showTextEng\" onclick=\"showEnglish()\">Show English</a>"
    "<a href=\"javascript:void(0)\" class=\"hideTextEng\" onclick=\"hideEnglish()\">Hide English</a>"
    "<a href=\"javascript:void(0)\" class=\"showTextBo\" onclick=\"showBo()\">Show Tibetan</a>"
    "<a href=\"javascript:void(0)\" class=\"hideTextBo\" onclick=\"hideBo()\">Hide Tibetan</a>"
    (reduce str ( create-chapter-links md))
    "</div>"
    "<span class=\"openBtn\" onclick=\"openNav()\">&#9776; open</span>"))


(defn make-main-body [md]
  (str "<div id=\"main\">" (reduce str md) "</div>"))

(defn add-navigation [md content]
  (let [menu (make-menu-div md)
        main (make-main-body content)]
    (str (reduce str menu) (reduce str main))))

(defn output-pprint [content output-filename]
  (spit output-filename (with-out-str (pprint/pprint content))))

(defn output-json [content output-filename]
  (spit output-filename (with-out-str (json/write-str content))))

(defn output-md [content output-filename]
  (spit output-filename (str  (reduce str content) " " row-normal-image)))

(defn output-md-with-navigation [content md output-filename]
  (spit output-filename (add-navigation md (str  (reduce str content) " " row-normal-image))))






