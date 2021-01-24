(ns split-text.markdown
  (:require
    [split-text.config :refer :all]
    [clojure.string :as str]
    [split-text.io :refer :all]
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
  (select [ALL FIRST] (re-seq #"((?:[-0-9]+)(\D+|[0-9]+,000|[0-9]+,[0-9]+|[0-9]{3}|[0-9]\:[\-0-9]+)*)" verstr)));;((?:[-0-9]+)\D+);;((?:[-0-9]+) (\D+|[0-9]+,000)*)

(defn handle-text [t]
  "looks for lines of text that are deemed as text - they are not headings or blanks"
  (loop [[ex & rx :as allt] t ox ()]
    (cond (and (or(nil? ex)(empty? ex)) (not (nil? rx))) (recur rx ox)
          (and (nil? ex) (nil? rx))  (into (list (split-verses (str/join "_" (flatten ox))) allt))
          :else (let [header1? (re-find header-regex ex)]
                  (if header1?
                    (into (list (split-verses (str/join "_"(flatten ox))) allt))
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
    (cond (str/starts-with? (:text s) "#####") (assoc s :type :h5 :text (subs (:text s) 5))
          (str/starts-with? (:text s) "####") (assoc s :type :h4 :text (subs (:text s) 5))
          (str/starts-with? (:text s) "###") (assoc s :type :h3 :text (subs (:text s) 3))
          (str/starts-with? (:text s) "##") (assoc s :type :h2 :text (subs (:text s)  2))
          (str/starts-with? (:text s) "#") (assoc s :type :h1 :text (subs (:text s)  1))
          :else (assoc s :type :verse))))

(defn index-lines [mt]
  (map-indexed (fn [idx itm] (assoc itm :index idx)) mt))

(defn merge-quotations [mt]
  (loop [  [one two & remaining] mt  o []]
    ;(println one o)
    (cond (and (= (:type one) :verse) (= (:type two) :h4))
          (recur (let [ newone (if (contains? one :fulltext)
                                 (assoc one :fulltext (assoc (:fulltext one) (:index two) (str "> " (:text two))))
                                 (assoc one :fulltext  (assoc {} (:index one) (:text one) (:index two) (str "> " (:text two)))))]
                   ((comp vec flatten conj) [] newone (flatten remaining))) o)
          (and (nil? one) (nil? two)) o
          (nil? two) (let [n (if (not (contains? one :fulltext)) (assoc one :fulltext (assoc {} (:index one) (:text one))) one)]
                       (recur two (conj o (assoc n :fulltext  (:fulltext n)))))
          :else (let [n (if (not (contains? one :fulltext)) (assoc one :fulltext (assoc {} (:index one) (:text one))) one)]
                  (recur ((comp vec flatten conj) [] two remaining)
                         (conj  o (assoc n :fulltext (:fulltext n))))))))

(defn replace_underlines [l]
  (let [l1 (str/replace (str/replace l #"\\\[" "\\@") #"\\\]" "\\#")
        l2 (str/replace l1 #"(\[([^\[].*?)\]\{\.underline\})" split-text.config/name-highlight)
        l3 (str/replace (str/replace l2 #"\@" "\\[") #"\#" "\\]")]
    l3))
(defn transform-underlines [md]
  (vec(transform [ALL #(or (= (:type %) :verse) (= (:type %) :h4)) :text] replace_underlines md)))

(defn remove_underlines [l]
  (let [l1 (str/replace (str/replace l #"\\\[" "\\@") #"\\\]" "\\#")
        l2 (str/replace l1 #"(\[([^\[].*?)\]\{\.underline\})" split-text.config/name-highlight-h2)
        l3 (str/replace (str/replace l2 #"\@" "\\[") #"\#" "\\]")]
    l3))

(defn transform-heading-underlines [md]
  (vec(transform [ALL #(or (= (:type %) :h3) (= (:type %) :h5)) :text] remove_underlines md)))





(defn handle-dir-rtl [l]
  (str/replace l #"\[ \]\s*\{dir=.rtl.\}" " "))

(defn remove-dir-rtl [md]
  (vec (transform [ALL #(= (:lang %) :bo) :text] handle-dir-rtl md)))

(defn handle-verse-number [l]
  (let [text (:text l)
        matcher (re-matcher #"^(\d+[-\d]*)(?:\s*)?(.*)" text)
        matches (re-find matcher)
        verse-number (second matches)
        verse-text (last matches)
        new-l (assoc l :text verse-text :verse-number verse-number)]
    new-l))


(defn transform-verse-numbers [md]
  (vec(transform [ALL #(= (:type %) :verse)  ] handle-verse-number md)))


(defn handle-h4-verse-number [l]
  (let [text (:text l)
        matcher (re-matcher #"^(\d+[-\d]*)(?:\s*)?(.*)" text)
        matches (re-find matcher)]
    (if (some? matches) ;; verse number exists so process
      (let [verse-number (second matches)
            verse-text (last matches)
            new-l (assoc l :text verse-text :verse-number verse-number)]
        new-l)
      l))) ;; else just return the original

(defn transform-h4-verse-number [md]
  (vec(transform [ALL #(= (:type %) :h4)  ] handle-h4-verse-number md)))

(defn handle-sentence-spaces [l]
  (let [; space between normal sentances
        l1 (str/replace l #"(\u0F0D|&#xf0d;|\u0F42)(\)|\"|&quot;)?(?!$)(\s+)(?!$)" sentence-space-format)
        ; space after ga with she at start of next sentence
        l2 (str/replace l1 #"(\u0F42)(\u0F0D)" sentence-ka-she-format)
        ; space after She followed by bracket
        l3 (str/replace l2 #"(\u0F0D])" sentence-she-bracket-format)
        ; space (ensp) after colon
        l4 (str/replace l3 #"(:)(?:\s*)?" sentence-colon-format)]
    l4))

(defn transform-sentence-space [md]
  (vec(transform [ALL #(= (:lang %) :bo) :text] handle-sentence-spaces md)))

(defn handle-spaces-after-bo-brackets [l]
  (let [l1 (str/replace l #"(?:\s+)?(\(|\[)" "$1")
        l2 (str/replace l1 #"(\)|\])(\s+)?" "$1\u200A")]
    l2))

(defn transform-spaces-after-bo-brackets [md]
  (vec(transform [ALL #(= (:lang %) :bo) :text] handle-spaces-after-bo-brackets md)))

(defn handle-joined-lines [e]
  (let [l (:text e)]
    (if (= (:lang e) :bo)
        (assoc e :text (str/replace (str/replace l #"\u0F0D_" "&#xf0d;    ") #"_" ""))
        (assoc e :text (str/replace l #"_" " ")))))


(defn transform-joined-lines [md]
  (vec(transform [ALL] handle-joined-lines md)))

(defn handle-quotes [e]
  (let [l (:text e)]
    (if (= (:lang e) :bo)
      (assoc e :text (str/replace (str/replace l "\"" "" ) "'" "")) ;"&quot;" "&apos;"
      (assoc e :text l))))


(defn transform-quotes [md]
  (vec(transform [ALL] handle-quotes md)))



(defn handle-bo-lines [e]
  (let [l (:text e)]
    ;(do ;(tap> l)
    (if (= (:lang e) :bo)
      (assoc e :text (str/replace l #"([\u0F00-\u0FDA]+ *[\u0F00-\u0FDA]*)*" "<span lang=\"bo\">$1</span>"))
      ;(assoc e :text (str/replace l #"(([\u0F00-\u0FDA]+[ ]*)*)" "<span lang=\"bo\">$1</span>"))
      (assoc e :text l))))


(defn mark-bo-lang-lines [md]
  (vec(transform [ALL] handle-bo-lines md)))

(defn filter-verses [l]
  ;(or
  (and (= (:lang l) :bo) (contains? #{ :verse } (:type l))))  ;(= (:type l) :h1)))

(defn get-verses [md]
  (filter filter-verses md))


(defn get-verse-index [md]
  ( into {} (let [x (partition 3 (select [ALL (multi-path :index :chapter :verse-number)] (get-verses md)))]
              (for [[k c v] x]
                (assoc {} k [c (str/trim v)])))))

(defn get-verse-for-h3-entry [indexes entry]
  (let [verse (first (filter #(< (:index entry) (key %)) indexes))]
    (last (last verse))))

(defn set-next-verse [indexes e]
  (assoc e :next-verse (get-verse-for-h3-entry indexes e)))



(defn allocate-h3-next-verse [md]
  (let [indexes (sort (get-verse-index md))
        set-h3-next-verse (partial set-next-verse indexes)]
    (transform  [ALL #(= (:type %) :h3)] set-h3-next-verse md)))



(defn get-chapter-entries [md]
  (filter filter-chapter-eng-headings md))

(defn get-chapters [md]
 ( into {} (let [x (partition 2 (select [ALL (multi-path :index :text)] (get-chapter-entries md)))]
            (for [[k v] x]
              (assoc {} k (str/trim v))))))

(defn get-indexes [chapters]
  (sort(keys chapters)))

(defn get-chapter-for-entry [chapters indexes entry]
  (let [chapter-index (last (filter #(>= (inc (:index entry)) %) indexes))]
    (get chapters (if (nil? chapter-index) (first indexes) chapter-index))))



(defn allocate-chapters [md]
  (let [chapters (get-chapters md)
        indexes (get-indexes chapters)]
    (map #(assoc % :chapter (get-chapter-for-entry chapters indexes %)) md)))




(defn process-markdown [md]
  (-> md
      (clean-markdown)
      (flatten)
      (merge-lines)
      (classify-language)
      (classify-type)
      (index-lines)
      (transform-underlines)
      (transform-heading-underlines)
      (transform-quotes)
      ;(mark-bo-lang-lines)
      (transform-joined-lines)
      (transform-verse-numbers)
      (allocate-chapters)
      (allocate-h3-next-verse)
      (transform-h4-verse-number)
      (remove-dir-rtl)
      (transform-sentence-space)
      (transform-spaces-after-bo-brackets)
      (merge-quotations)))

(defn process-markdown-file [filename]
  (-> (read-markdown filename)
      (process-markdown)))

(defn filter-markdown-for-eng [l]
  (and (= (:lang l) :english) (contains? #{ :verse :h1 :h2 :h3 :h4 :h5} (:type l))))

(defn filter-markdown-for-bo [l]
  (or (and (= (:lang l) :bo) (contains? #{ :verse :h1 :h2 :h3 :h4 :h5} (:type l)))
      (and    (= (:lang l) :english) (contains? #{ :h2 :h1} (:type l)))))

(defn filter-markdown-for-boeng [l]
  (or (and (contains? #{:bo :english} (:lang l)) (contains? #{:verse :h1 :h2 :h3 :h4 :h5} (:type l)))  (= (:type l) :h1)))

(defn wrap-verse-in-span [l lang]
  "Wrap a line that starts with a verse number in a span with class of lang"
  (if (str/starts-with? l "<span>")
    (let [end-of-vn-span (+ 7 (str/index-of l "</span>"))
          sol (subs l 0 end-of-vn-span)
          span (subs l end-of-vn-span)]
      (str sol "<span class=\"v-" (name lang) "\">" span "</span>\n"))
    (str "<span class=\"v-" (name lang) "\">" l "</span>\n")))

(defn wrap-quote-in-span [l lang]
  "Wrap a line that starts with a verse number in a span with class of lang"
  (if (str/starts-with? l "<span>")
    (let [end-of-vn-span (+ 7 (str/index-of l "</span>"))
          sol (subs l 0 end-of-vn-span)
          span (subs l end-of-vn-span)]
      (str sol "<span class=\"vq-" (name lang) "\">" span "</span>\n"))
    (str "<span class=\"vq-" (name lang) "\">" l "</span>\n")))

(defn handle-bo-brackets [l]
  (str/replace l #"(\[\d+\]|[\(\)\[\]\:]|\d+\:\d+(\-\d+)?|[0-9]+,[0-9]+|666|216|an ERV paraphrase|\&apos;|\&quot;)" bo-brackets))

(defn transform-underline-style-line [l]
  (str/replace l #"(\{(.+?)\})" split-text.config/name-highlight-style))

(defn process-text [lang text]
  (if (= lang :bo)
    (->  text
         (handle-bo-brackets)
         (transform-underline-style-line))
    text))

(defn h1 [_ l]
  (let [lang (:lang l)
        text (process-text lang (:text l))]
    (str "<h1 class=\"h1-" (name lang) "\">" text "</h1>\n")))

(defn h2 [style l]
  (let [lang (:lang l)
        text (:text l)]
    (cond (and (= style :boeng-cols) (= lang :english))
          (str row-tiny-image "<div><h2 id=\"" (str/trim text) "\" class=\"h3-" (name lang) "\">" text "</h2>" "</div>\n") ;; h3-english works for two cols
          (and (or (= style :boeng)(= style :bo)) (= lang :bo))
          (str  row-tiny-image "<div><h2 id=\"" (str/trim text) "\" class=\"h2-" (name lang) "\">" text "</h2>" "</div>\n")
          :else (str "<div><h2 class=\"h2-" (name lang) "\">" text "</h2>\n"))))

(defn h3 [_ l]
  (let [lang (:lang l)
        text (process-text lang (:text l))]
    (str "<h3 class=\"h3-" (name lang) "\">" text "</h3>\n")))

(defn h5 [_ l]
  (let [lang (:lang l)
        text (process-text lang (:text l))]
    (str "<h5 class=\"h5-" (name lang) "\">" text "</h5>\n")))

(defn h4 [_ l]
  (let [lang (:lang l)
        text (:text l)
        vn (str "<span class=\"vn\">" (:verse-number l) "</span>")
        itext (wrap-quote-in-span (process-text lang (str vn " " text)) lang)]
    (str "<div class=\"q-" (name lang) "\">" itext "</div>\n")))

(defn verse [_ l]
  (let [lang (:lang l)
        text (:text l)
        vn (str "<span class=\"vn\">" (:verse-number l) "</span>")
        itext (wrap-verse-in-span (process-text lang (str vn " " text)) lang)]
    (str "<div class=\"p-" (name lang) "\">" itext "</div>\n")))

(defn output-markdown [style md]
        (for [l md]
          (let [ftype (resolve (symbol (:type l)))
                ;x (println "!" lang "!" type "!" text "!")
                out (ftype style l)]
            out)))


(defn output-div-pairs [md]
  (let [header (take-while #(= (:type %) :h1) md)
        outputhead (reduce str(output-markdown :boeng-cols header))
        body (drop (count header) md)
        outputbody (loop  [[left right & rest] body output ""]
                     (if (empty? left)
                       output
                       (let [spans  (reduce str (doall (output-markdown :boeng-cols (into [] (list right left)))))
                             div (str "<div class=\"verse\">" spans "</div>\n")]
                         (recur rest (str output div)))))]
    (str outputhead outputbody)))


(defn output-bo-markdown [md]
  (output-markdown :bo (filter filter-markdown-for-bo md)))

(defn output-eng-markdown [md]
  (output-markdown :eng (filter filter-markdown-for-eng md)))

(defn output-boeng-markdown [md]
  (output-markdown :boeng (filter filter-markdown-for-boeng md)))

(defn output-boeng-interlinear [md]
  (output-div-pairs (filter filter-markdown-for-boeng md)))


;; post processing
;pandoc -s .\ProcessJames.md -c .\resources\css\main.css --metadata title="James" -o ProcessJamesBo.html

;C:\Users\MartinRoberts\AppData\Local\Pandoc\pandoc -s "James.out.md" -A .\resources\html\footer.html -c .\resources\css\main.css    -o "James.html"