(ns split-text.markdown
  (:require
    [split-text.config :refer :all]
    [clojure.string :as str]
    [com.rpl.specter :refer :all]))



(def header-regex #"#{1,3}.*")
(def blank-regex #"^\s$")
(def text-regex #"^[^#]*")

(defn read-markdown [filename]
  (line-seq(clojure.java.io/reader filename)))

(defn reduce-split-lines [x y]
  (if  (= (last y) \[)
    (assoc x :partial (str/join ""  [(:partial x) y]))
    (if (= (second y) \\)
      (assoc x :strings (into (:strings x)  (list(str/join "" [(:partial x) y]))) :partial nil)
      (assoc x :strings (into (:strings x) (list y)) :partial nil))))

(defn merge-lines [ls]
  (let [new (reduce reduce-split-lines {:strings [] :partial nil} ls)]
    (:strings new)))

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


(defn classify-language [ml]
  (for [s ml]
    (cond (some? (some #(if (>= (int %) 3840) %) s)) {:text s :lang :bo}
          (str/includes? s "/") {:text s :lang :back}
          :else {:text s :lang :english})))

(defn classify-type [mt]
  (for [s mt]
    (cond (str/starts-with? (:text s) "###") (assoc s :type :h3 :text (subs (:text s) 3))
          (str/starts-with? (:text s) "##") (assoc s :type :h2 :text (subs (:text s)  2))
          (str/starts-with? (:text s) "#") (assoc s :type :h1 :text (subs (:text s)  1))
          :else (assoc s :type :verse))))

(defn index-lines [mt]
  (map-indexed (fn [idx itm] (assoc itm :index idx)) mt))

(defn replace_underlines [l]
  (str/replace l #"(\[{1}([^\[].*?)\]\{\.underline\})" split-text.config/name-highlight))

(defn transform-underlines [md]
  (vec(transform [ALL :text] replace_underlines md)))

(defn process-markdown [md]
  (-> md
    (clean-markdown)
    (flatten)
    (merge-lines)
    (classify-language)
    (classify-type)
    (index-lines)
    (transform-underlines)))


(defn process-markdown-file [filename]
  (-> (read-markdown filename)
      (process-markdown)))

(defn filter-markdown-for-bo [l]
  (or (and (= (:lang l) :bo) (contains? #{ :verse :h2 :h3} (:type l)))  (= (:type l) :h1)))

(defn output-markdown [md]
  (for [l md]

    (let [text (:text l)
          x (println text)
          type (:type l)
          out (cond (= type :h1) (str "# " text "\n")
                    (= type :h2) (str "## " text "\n")
                    (= type :h3) (str "### " text "\n")
                    (= type :verse) (str text "\n\n"))]
      out)))

(defn output-bo-markdown [md]
  (output-markdown (filter filter-markdown-for-bo md)))

;; post processing
;pandoc -s .\ProcessJames.md -c .\resources\css\main.css --metadata title="James" -o ProcessJamesBo.html