(ns split-text.io
  (:require [split-text.config :refer :all]
            [split-text.css :refer :all]
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

(defn get-file [fn]
  (clojure.java.io/file fn))

(defn file-epoch [fn]
   (.lastModified (get-file fn)))

(defn file-exists? [fn]
  (.exists (get-file fn)))

(def lastModified (partial file-epoch))

(defn as-hickory-read-file [filename]
  (let [raw (clean (slurp filename))
        parsed-doc (h/parse raw)]
    (h/as-hickory parsed-doc)))

(defn filter-chapter-bo-headings [l]
  ;(or
  (and (= (:lang l) :bo) (contains? #{ :h2 } (:type l))))  ;(= (:type l) :h1)))

(defn filter-chapter-eng-headings [l]
  ;(or
  (and (= (:lang l) :english) (contains? #{ :h2 } (:type l))))  ;(= (:type l) :h1)))

(defn create-chapter-links [md]
  (let [b (filter filter-chapter-bo-headings md)
        a (filter filter-chapter-eng-headings md)
        chapters (map (fn [ a b ] {:menu (str a b) :dest b}) (map :text a ) (map :text b))]

    (for [c chapters]
      (str "<a href=\"#" (str/trim (:dest c)) "\">" (str/trim(:menu c)) "</a>"))))


(defn make-menu-div [md style]
  (str
    "<div id=\"mySidenav\" class=\"sidenav\">"
    "<a href=\"javascript:void(0)\" class=\"closebtn\" onclick=\"closeNav()\">&times;</a>"
    (if (= "boeng-nav" style)
      (str "<a href=\"javascript:void(0)\" class=\"showTextEng\" onclick=\"showEnglish()\">Show English</a>"
           "<a href=\"javascript:void(0)\" class=\"hideTextEng\" onclick=\"hideEnglish()\">Hide English</a>"
           "<a href=\"javascript:void(0)\" class=\"showTextBo\" onclick=\"showBo()\">Show Tibetan</a>"
           "<a href=\"javascript:void(0)\" class=\"hideTextBo\" onclick=\"hideBo()\">Hide Tibetan</a>"))

    (reduce str ( create-chapter-links md))
    "</div>"
    "<span class=\"openBtn\" onclick=\"openNav()\">&#9776; open</span>"))


(defn make-main-body [md]
  (str "<div id=\"main\">" (reduce str md) "</div>"))

(defn add-navigation [md style content]
  (let [menu (make-menu-div md style)
        main (make-main-body content)]
    (str (reduce str menu) (reduce str main))))

(defn output-pprint [content output-filename]
  (spit output-filename (with-out-str (pprint/pprint content))))

(defn output-json [content output-filename]
  (spit output-filename (with-out-str (json/write-str content))))

(defn output-md [content output-filename]
  (spit output-filename (str  (reduce str content) " " row-normal-image)))

(defn get-html-header []
  (let [header-open "<head>\n  <meta charset=\"utf-8\" />\n  <meta name=\"generator\" content=\"pandoc\" />\n  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, user-scalable=yes\" />\n  <title>2020-Revelation-Finalint.out</title>"
        header-close "</head>"]
    (str header-open main-css header-close)))




(defn wrap-html [header content]
  (str "<html>\n" header "<body>\n" content "</body>\n</html>"))

(comment
  (get-html-header)
  ,)


(defn output-html [ content output-filename]
  (let [header (get-html-header)
        html (wrap-html header (str  (reduce str content) " " row-normal-image))]
    (spit output-filename html)))

(defn output-md-with-navigation [content md style output-filename]
  (spit output-filename (add-navigation md style (str  (reduce str content) " " row-normal-image))))






