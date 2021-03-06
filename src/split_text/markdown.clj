(ns split-text.markdown
  (:require
    [split-text.config :as conf]
    [split-text.db :as db]
    [split-text.wylie :as wy]
    [clojure.string :as str]
    [clojure.java.io :as io]
    [com.rpl.specter :refer [select transform ALL FIRST multi-path]]
    [split-text.io :as spio]))



(def header-regex #"#{1,3}.*")
(def blank-regex #"^\s$")
(def text-regex #"^[^#]*")

(defn read-markdown [filename]
  (let [text (with-open [r (io/reader filename)]
               (doall (line-seq r)))]
    text))



(defn reduce-split-lines [x y]
  (if  (= (last y) \[)
    (assoc x :partial (str/join ""  [(:partial x) y]))
    (if (= (second y) \\)
      (assoc x :strings (into (:strings x) (list(str/join "" [(:partial x) y]))) :partial nil)
      (assoc x :strings (into (:strings x) (list y)) :partial nil))))

(defn merge-lines [ls]
  (let [new (reduce reduce-split-lines {:strings [] :partial nil} ls)]
    (:strings new)))

(defn split-verses [verstr]
  (if (:verse-numbers @conf/config-atom)
    (let [regex (if (str/includes? verstr "(24)") conf/split-verse-24 conf/split-verse-normal)
          parts (select [ALL FIRST] (re-seq regex verstr))]
      parts)
    verstr))
        ;;((?:[-0-9]+)\D+);;((?:[-0-9]+) (\D+|[0-9]+,000)*)

(defn handle-text
  "looks for lines of text that are deemed as text - they are not headings or blanks"
  [t]
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
  (let [languages (:languages @conf/config-atom)]
    (tap> languages)
    (for [s ml]
      (cond (and (some #(= :bo %) languages) (some? (some #(when (>= (int %) 3840) %) s))) {:text s :lang :bo}
          (and (some #(= :back %) languages) (str/includes? s "/")) {:text s :lang :back}
          (some #(= :english %) languages) {:text s :lang :english}
          (some #(= :back %) languages) {:text s :lang :back}))))

(defn classify-type [mt]
  (for [s mt]
    (do (conf/debug s)
        (cond (nil? s) s
              (str/starts-with? (:text s) "#####") (assoc s :type :h5 :text (subs (:text s) 5))
              (str/starts-with? (:text s) "####") (assoc s :type :h4 :text (subs (:text s) 5))
              (str/starts-with? (:text s) "###") (assoc s :type :h3 :text (subs (:text s) 3))
              (str/starts-with? (:text s) "##") (assoc s :type :h2 :text (subs (:text s) 2))
              (str/starts-with? (:text s) "#") (assoc s :type :h1 :text (subs (:text s) 1))
              :else (assoc s :type :verse)))))

(defn index-lines [mt]
  (map-indexed (fn [idx itm] (assoc itm :index idx)) mt))

(defn merge-quotations
  " Quotations exist on separate lines and these needs to be associated with the appropriate
    Verse.  This is done by walking through the set of lines and when a quotation is found it
    is added to the last found verse line."
  [mt]
  (loop [  [one two & remaining] mt  o []]
    ;(println "first check:" (and (contains? one :verse-number) (= (:type two) :h4) (= (:lang one) (:lang two))))
    (cond (and (contains? one :verse-number) (and (= (:type two) :h4) (not (contains? two :verse-number)))  (= (:lang one) (:lang two))) ; quotation exists
          (recur (let [ newone (if (contains? one :fulltext)
                                 (assoc one :fulltext (assoc (:fulltext one) (:index two) (str "> " (str/trim (:text two)))))
                                 (assoc one :fulltext  (assoc {} (:index one) (if (= (:type one) :h4) (str "> " (str/trim (:text one))) (:text one)) (:index two) (str "> " (:text two)))))]
                   ((comp vec flatten conj) [] newone (flatten remaining))) o)
          (and (nil? one) (nil? two)) o ; final exit
          (nil? two) (let [n (if (not (contains? one :fulltext))
                               (assoc one :fulltext (assoc {} (:index one) (if (= (:type one) :h4) (str "> " (str/trim (:text one))) (:text one)))) one)]
                       (recur two (conj o (assoc n :fulltext  (:fulltext n)))))
          :else (let [n (if (not (contains? one :fulltext)) (assoc one :fulltext (assoc {} (:index one) (if (= (:type one) :h4) (str "> " (str/trim (:text one))) (:text one)))) one)]
                  (recur ((comp vec flatten conj) [] two remaining)
                         (conj  o (assoc n :fulltext (:fulltext n))))))))

(defn replace_underlines [l]
  (let [l1 (str/replace (str/replace l #"\\\[" "\\@") #"\\\]" "\\#")
        l2 (str/replace l1 #"(\[([^\[].*?)\]\{\.underline\})" conf/name-highlight)
        l3 (str/replace (str/replace l2 #"\@" "\\[") #"\#" "\\]")]
    l3))
(defn transform-underlines
  "replace the markdown format of underline into a set of highights for names"
  [md]
  (vec(transform [ALL #(or (= (:type %) :verse) (= (:type %) :h4)) :text] replace_underlines md)))

(defn replace-smallcaps [l]
  (let [l1 (str/replace (str/replace l #"\\\[" "\\@") #"\\\]" "\\#")
        l2 (str/replace l1 #"(\[([^\[].*?)\]\{\.smallcaps\})" conf/extra-format)
        l3 (str/replace (str/replace l2 #"\@" "\\[") #"\#" "\\]")]
    l3))
(defn transform-smallcaps
  "replace the markdown format of underline into a set of highights for names"
  [md]
  (vec(transform [ALL  :text] replace-smallcaps md)))

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
    (vec (transform [ALL #(= (:type %) :verse)] handle-verse-number md)))


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
    (vec (transform [ALL #(= (:type %) :h4)] handle-h4-verse-number md)))

(defn handle-sentence-spaces [l]
  (let [res (-> l
                  ; space between normal sentances
                  (str/replace #"(\u0F0D|&#xf0d;|\u0F42)(\"|&quot;)?(?!$)(\s+)(?!$)" "$1$2&ensp;")
                ;  she before a bracket
                  (str/replace  #"(།|u0F0D|&#xf0d;)(\[|\()" "$1&ensp;$2")
                  ; space after ga with she at start of next sentence
                  (str/replace  #"(\u0F0D)(\)|\])" "$1$2&ensp;")
                  ; space after ga with she at start of next sentence
                  (str/replace  #"(\u0F42)(\u0F0D)" "$1&ensp;$2")
                  ;  space after ga closing bracket with she at start of next sentence
                  (str/replace  #"(\u0F42)(\]|\))(\u0F0D)" "$1$2&ensp;$3")
                  ;  space after ga closing bracket with she at start of next sentence
                  (str/replace  #"(\u0F42)(\]|\)) (\u0F0D)" "$1$2&ensp;$3")
                  ;  space after ga opening bracket with she at start of next sentence
                  (str/replace  #"(\u0F42)(\[|\()(\u0F0D)" "$1&ensp;$2$3")
                  ;  space after ga opening bracket with no she at start of next sentence
                  (str/replace  #"(\u0F42)(\[|\()" "$1&ensp;$2")
                  ; space after She followed by bracket
                  (str/replace  #"(\u0F0D])" "$1&ensp;")
                  (str/replace  #"(\u0F0D\)) " "$1&ensp;")
                  ;remove colons after shed
                  (str/replace  #"(།|\u0F0D|&#xf0d;):" "$1&ensp;")
                  ; remove colons after ga at end of text
                  (str/replace  #"(ག|\u0F42):(.)?$" "$1")
                  ; remove colons after ga
                  (str/replace  #"(ག|\u0F42):(\s)?" "$1&ensp;།")
                  ; remove final colon
                  (str/replace  #":(\s)" "")
                  ; remove any stray spaces
                  (str/replace  #"(\s)" "")
                  ; replace &#xf0d; with she
                  (str/replace #"(\u0F0D|&#xf0d;)" "།")
                  ; replace ka entity
                  (str/replace #"\u0F42" "ག")
                  (str/replace #"&ensp;&ensp;\u200A"  "&ensp;"))]
    res))

(defn transform-sentence-space [md]
  (vec(transform [ALL #(= (:lang %) :bo) :text] handle-sentence-spaces md)))

(defn handle-markdown-stars [l]
  (let [l1 (str/replace l #"\\\*" "#")
        l2 (str/replace l1 #"[*]" "")
        l3 (str/replace l2 #"#" "\\\\*")]
      l3))

(comment
  (str/replace "djdjd\\*djdj" #"\\\*" "#")
  (str/replace "sjsjsj#*ssjsj" #"\*" "")
  (str/replace "sjsjsj#ssjsj" #"#" "\\\\*")
  (handle-markdown-stars "djdjd\\*djd*j")

  ,)


(defn remove-markdown-stars [md]
  (vec(transform [ALL  :text] handle-markdown-stars md)))


(defn handle-spaces-after-bo-brackets [l]
  (-> l
      (str/replace  #"(?:\s+)?(\(|\[)" "$1")
      (str/replace  #"(\)|\])(\s+)?" "$1\u200A")))

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
    (contains? #{ :verse } (:type l)))
;(= (:type l) :h1)))
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
      (transform [ALL #(= (:type %) :h3)] set-h3-next-verse md)))



(defn get-chapter-entries [md]
  (let [eng (filter spio/filter-chapter-eng-headings md)]
    (conf/debug "ENG" eng)
    (if (empty? eng) (filter spio/filter-chapter-back-headings md)
                     eng)))

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

(defn prepare-for-db [source book lang md]
  (map #(db/make-entry source book %) md))


(defn process-markdown-1 [md]
  (let [out (-> md
                (clean-markdown)
                (flatten)
                (merge-lines)
                (classify-language)
                (classify-type)
                (index-lines)
                (allocate-chapters))]
    (when (:Test @conf/config-atom) (spit (str (:directory @conf/config-atom) "\\firstpass.out" ) (reduce str out)))
    out))


(defn process-markdown-2 [md]
  (if (:verse-numbers @conf/config-atom)
    (do (tap> "verses")
        (-> md
            (transform-underlines)
            (transform-smallcaps)
            (transform-heading-underlines)
            (transform-quotes)
            (remove-markdown-stars)
            ;(mark-bo-lang-lines)
            (transform-joined-lines)
            (transform-verse-numbers)
            (allocate-h3-next-verse)
            (transform-h4-verse-number)
            (remove-dir-rtl)
            (transform-spaces-after-bo-brackets)
            (transform-sentence-space)
            (merge-quotations)))
    (do
      (tap> "No Verses")
      (-> md
          (transform-underlines)
          (transform-heading-underlines)
          (transform-quotes)
          (remove-markdown-stars)
          (transform-joined-lines)
          (remove-dir-rtl)
          (transform-sentence-space)
          (transform-spaces-after-bo-brackets)
          (merge-quotations)))))



(defn process-markdown-file [filename book source]
  (let [pass1 (-> (read-markdown filename)
                  (process-markdown-1))
        grouped (group-by :lang pass1)
        title (:title @conf/config-atom)]
    (for [[k v] grouped]
      (let [md (process-markdown-2 v)
            dbmd (prepare-for-db source book k md)]
        (if (:Test @conf/config-atom)
          (do (conf/debug "Testing")
              (conf/debug (str (:directory @conf/config-atom) "\\" book "_" (name k) ".out"))
              (when (= k :bo)
                (let [w-entries (wy/get-wylie-post-full-text md)
                      w-dbmd (prepare-for-db source book :wylie w-entries)]
                  (spit (str (:directory @conf/config-atom) "\\" book "_wylie.out" ) (reduce str w-dbmd))))
              (spit (str (:directory @conf/config-atom) "\\" book "_" (name k) ".out" ) (reduce str dbmd)))
          (let [transaction (db/add_entries db/conn source book dbmd)]
               (conf/debug transaction)
               (when (= k :bo)
                 (do (conf/debug "inserting wylie")
                     (let [w-entries (wy/get-wylie-post-full-text md)
                           w-dbmd (prepare-for-db source book :wylie w-entries)
                           w-transaction (db/add_entries db/conn source book w-dbmd)
                           x (prn (first w-dbmd))]
                       (prn w-transaction)
                       (nil? w-transaction))))
               (prn transaction)
               (nil? transaction)))))))