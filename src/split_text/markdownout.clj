(ns split-text.markdownout
  (:require
    [split-text.config :refer :all]
    [split-text.db :refer :all]
    [split-text.css :refer :all]
    [clojure.string :as str]
    [split-text.io :refer :all]
    [com.rpl.specter :refer :all]))



(defn filter-markdown-for-eng [l]
  (and (= (:lang l) :english) (contains? #{:verse :h1 :h2 :h3 :h4 :h5} (:type l))))

(defn filter-markdown-for-bo [l]
  (or (and (= (:lang l) :bo) (contains? #{:verse :h1 :h2 :h3 :h4 :h5} (:type l)))
      (and (= (:lang l) :english) (contains? #{:h2 :h1} (:type l)))))

(defn filter-markdown-for-boeng [l]
  (or (and (contains? #{:bo :english} (:lang l)) (contains? #{:verse :h1 :h2 :h3 :h4 :h5} (:type l))) (= (:type l) :h1)))




(defn handle-bo-brackets [l]
  (let [l1 (str/replace l #"\\\*" "<span class=\"bo-ref\">*</span>")]
    (str/replace l1 #"(\[\d+\]|[\(\)\[\]\:]|\d+\:\d+(\-\d+)?|[0-9]+,[0-9]+|666|216|24|an ERV paraphrase|3:1-15)" bo-brackets)))


(defn transform-underline-style-line [l]
  (str/replace l #"(\{(.+?)\})" split-text.config/name-highlight-style))

(defn wrap-verse-in-span [lang text]
  "Wrap a line that starts with a verse number in a span with class of lang"
  (if (str/starts-with? text "> ")
    text
    (str "<span class=\"v-" (name lang) "\">" text "</span>\n")))


(defn wrap-quote-in-span [lang text]
  (if (str/starts-with? text "> ")
    (str "> <span class=\"vq-" (name lang) "\">" (subs text 2) "</span>\n")
    text))


(defn handle-quotations [l]
  (if ignore-quotations?
    (let [pass1 (str/join (select [:lines ALL :line] l))
          pass2 (str/replace pass1 #"^\> " "")]
      pass2)
    (let [wrapquote (partial wrap-quote-in-span (:lang l))]
      (transform [:lines ALL :line] wrapquote l))))

(defn handle-verse [l]
  (let [wrapverse (partial wrap-verse-in-span (:lang l))]
    (transform [:lines ALL :line] wrapverse l)))

(defn process-bo-text [text]
  (-> text
      (handle-bo-brackets)
      (transform-underline-style-line)))

(defn add-verse-number [verse-number text]
  (if (str/starts-with? text "> ")
    (str "> " "<span class=\"vn\">" verse-number "</span> " (subs text 2))
    (str "<span class=\"vn\">" verse-number "</span> " text)))


(defn attach-verse-number [l]
  (let [min-lineno (apply min (select [:lines ALL :line-no] l))
        add-vn (partial add-verse-number (:verse-number l))]
    (transform [:lines ALL #(= (:line-no %) min-lineno) :line] add-vn l)))

(defn wrap-in-div [lang text]
  (if (str/starts-with? text "> ")
    (str "<p class=\"pq-" lang "\">" (subs text 2) "</p>")
    (str "<p class=\"p-" lang "\">" text "</p>")))

(defn join-lines [l]
  (let [wrap-divs (partial wrap-in-div (:lang l))
        pass1 (transform [:lines ALL :line] wrap-divs l)
        text (str/join (select [:lines ALL :line] pass1))]
    (assoc l :text-out text)))


(defn process-text [l]
  (let [lang (:lang l)
        pass1 (if (= lang "bo") (transform [:lines ALL :line] process-bo-text l) l)]
    (-> pass1
        (attach-verse-number)
        (handle-quotations)
        (handle-verse)
        (join-lines))))

(defn apply-class [l class]
  (let [lang (:lang l)
        text (first (select [:lines ALL :line] l))]
    (assoc l :text-out (str "<" class " class=\"" class "-" (name lang) "\">" text "</" class ">\n"))))


(defn verse [l]
  (let [lang (:lang l)
        text (process-text l)]
    text))

(defn h4 [l]
  (verse l))




(defn output-markdown [style md]
  ;(debug "om: "(first md))
  (for [l md]
    (if (= (:type l) :verse)
      (verse l)
      (apply-class l (name (:type l))))))


(defn output-div-pairs [md]
  (let [header (take-while #(= (:type %) :h1) md)
        outputhead (reduce str (output-markdown :boeng-cols header))
        body (drop (count header) md)
        outputbody (loop [[left right & rest] body output ""]
                     (if (empty? left)
                       output
                       (let [spans (reduce str (doall (output-markdown :boeng-cols (into [] (list right left)))))
                             div (str "<div class=\"verse\">" spans "</div>\n")]
                         (recur rest (str output div)))))]
    (str outputhead outputbody)))



(defn interleave-chapter-items [source-text]
  (let [source1-vn (select [ALL :verse-nos FIRST] (:s1 source-text))
        source2-vn (select [ALL :verse-nos FIRST] (:s2 source-text))
        heading-numbers1 (select [ALL :verse-nos FIRST] (:h1 source-text))
        heading-numbers2 (select [ALL :verse-nos FIRST] (:h2 source-text))]
    (loop [s1 source1-vn s2 source2-vn h1 heading-numbers1 h2 heading-numbers2 output []]
      (let [f (fn [x] (if (empty? x) 999 (first x)))
            S1 (f s1)
            S2 (f s2)
            H1 (f h1)
            H2 (f h2)]
        (debug "A: " H1 H2 S1 S2)
        (cond (and (empty? s1) (empty? s2))
              ;; exit loop
              output
              (<= H1 H2 S1 S2)
              (let [;x (debug "3: " H1 H2 S1 S2)
                    t [:h1 H1]]
                (recur s1 s2 (rest h1) h2 (conj output t)))
              (<= H2 S1 S2)
              (let [;x (debug "4: " H1 H2 S1 S2)
                    t [:h2 H2]]
                (recur s1 s2 h1 (rest h2) (conj output t)))
              (<= S1 S2)
              (let [;x (debug "1: " H1 H2 S1 S2)
                    t [:s1 S1]]
                (recur (rest s1) s2 h1 h2 (conj output t)))
              :else
              (let [;x (debug "2: " H1 H2 S1 S2)
                    t [:s2 S2]]
                (recur s1 (rest s2) h1 h2 (conj output t))))))))

#_(comment
    (count(fetch-verse-headings-verse-nos conn "Himlit" "Revelation" "bo" 2))
    (fetch-verse-nos conn "Himlit" "Revelation" "english" "2")

    (def S1 (fetch-chapter conn "Himlit" "Revelation" "bo" 2))
    (def S2 (fetch-chapter conn "Himlit" "Revelation" "english" 2))
    (def H1 (fetch-chapter-verse-headings conn "Himlit" "Revelation" "bo" 2))
    (def H2 (fetch-chapter-verse-headings conn "Himlit" "Revelation" "bo" 2))
    (def ch {:s1 S1 :s2 S2 :h1 H1 :h2 H2})
    (interleave-chapter-items ch)
    (select [ALL #(= (first (:verse-nos %)) 1)] (:s1 ch))
    (select [ALL :verse-nos FIRST] (:h1 ch))
    (:h1 ch)
    (> (first nil) (first [1]))
    (> nil 1)
    (if  1 2)
    (<= 1 3 4 5)
    ,)

(defn get-text [md]
  (let [text (output-markdown nil md)]
        ;x (debug "gt:" (first text))]
    (select [ALL :text-out] text)))

(defn order-source-text [items source_text]
  (for [item items]
    (select [ALL #(= (first (:verse-nos %)) (second item))] ((first item)  source_text))))

(defn get-source-text [source1-book source2-book chapter]
        {:s1 (select [ALL #(and (= (:chapter-no %) chapter) (= (:type %) :verse))] source1-book)
         :s2 (select [ALL #(and (= (:chapter-no %) chapter) (= (:type %) :verse))] source2-book)
         :h1 (select [ALL #(and (= (:chapter-no %) chapter) (= (:type %) :h3))] source1-book)
         :h2 (select [ALL #(and (= (:chapter-no %) chapter) (= (:type %) :h3))] source2-book)})






(defn get-chapter-text-interleaved [source1-book source2-book chapter]
  (let [source-text (get-source-text source1-book source2-book chapter)
        items (interleave-chapter-items source-text)
        ordered_source_text (order-source-text items source-text)
        text (get-text (flatten ordered_source_text))]
    text))

(defn get-book-interleaved [source1 lang1 source2 lang2 book]
  (let [source1-book (fetch-book conn source1 book lang1)
        source2-book (fetch-book conn source2 book lang2)
        both-books (flatten [ source1-book source2-book])
        chapters (sort(set(select [ALL :chapter-no] source1-book)))
        text (for [ch chapters]
               (let [header (get-text (select [ALL  #(and (= (:chapter-no %) ch) (= (:type %) :h2))] both-books))
                     chapter (get-chapter-text-interleaved source1-book source2-book ch)]
                 [row-tiny-image header chapter]))]
    text))

#_(defn handle-parallel-text [ source-text1 source-text2]
    (let [left-text (get-text source-text1)
          x (debug left-text)
          right-text (get-text source-text2)
          column1 (str column-div-open (reduce str (doall left-text)) div-close)
          column2 (str column-div-open (reduce str (doall right-text)) div-close)]
      (str "<div class=\"row\">" column1 column2 "</div>\n")))

(defn handle-parallel-text [ items source-text]
  (loop [[left right & rest] items output ""]
    (if (empty? left)
      output
      (let [left-text (select [ALL #(= (first (:verse-nos %)) (second left))] ((first left) source-text))
            right-text (select [ALL #(= (first (:verse-nos %)) (second right))] ((first right) source-text))
            left-output (first (get-text left-text))
            ;x (debug "row: " left-output)
            right-output (first (get-text right-text))
            div (str row-div-open column-div-open left-output div-close column-div-open right-output div-close div-close)]
            ;x (debug "row: " div)]
        (recur rest (str output div))))))


#_(defn get-chapter-text-parallel[source1-book source2-book chapter]
    (let [source-text1 (select [ALL #(and (= (:chapter-no %) chapter) (contains? #{:verse :h3} (:type %)))] source1-book)
          source-text2 (select [ALL #(and (= (:chapter-no %) chapter) (contains? #{:verse :h3} (:type %)))] source2-book)
          ordered_source_text (handle-parallel-text  source-text1 source-text2)]
      ordered_source_text))

(defn get-chapter-text-parallel [source1-book source2-book chapter]
  (let [x (debug "CH: " chapter)
        source-text (get-source-text source1-book source2-book chapter)
        items (interleave-chapter-items source-text)
        ordered_source_text (handle-parallel-text items source-text)]
    ordered_source_text))



(comment
  (def x [{:x 1 :c 2} {:x 2 :c 3} {:x 3 :c 1}])
  (select [ALL #(contains?  #{ 1 2} (:x %))] x)
  ,)

(defn get-book-parallel [source1 lang1 source2 lang2 book]
  (let [source1-book (fetch-book conn source1 book lang1)
        source2-book (fetch-book conn source2 book lang2)
        both-books (flatten [ source1-book source2-book])
        chapters (sort(set(select [ALL :chapter-no] source1-book)))
        text (for [ch chapters]
               (let [x (debug ch)
                     header (get-text (select [ALL  #(and (= (:chapter-no %) ch) (= (:type %) :h2))] both-books))
                     chapter (get-chapter-text-parallel source1-book source2-book ch)]
                 [row-tiny-image header chapter]))]
    text))

#_(comment
    (def source1-book (fetch-book conn "Himlit" "Revelation" "bo"))
    (def source2-book (fetch-book conn "Himlit" "Revelation" "english"))
    (def both-books (flatten [ source1-book source2-book]))
    (select [ALL  #(and (= (:chapter-no %) 2) (= (:type %) :h2)) :verse-number] both-books)
    (select [ALL #(and (= (:chapter-no %) 22) (= (:type %) :h3)) :verse-number] source1-book)
    (select [ALL  #(and (= (:chapter-no %) 22) (= (:type %) :h3))] both-books)
    (get-text (select [ALL  #(and (= (:chapter-no %) 2) (= (:type %) :h3))] both-books))
    (get-chapter-text-parallel source1-book source2-book 1)
    (for [items split-text.core/it] [l])
    (get-chapter-text-interleaved source1-book source2-book 22)
    (get-text(flatten (get-chapter-text-parallel source1-book source2-book 1)))
    (debug split-text.core/it)
    (concat [[2]] [[3]])
    ,)





(defn get-book-text [source book language]
  (let [chapters (fetch-chapter-numbers conn source book language)
        text (for [ch chapters]
               (let [header (get-text (fetch-chapter-header-lang conn "Himlit" book language ch))
                     chapter (get-text (fetch-chapter conn source book language ch))]
                 [row-tiny-image header chapter]))]
    text))

(comment
  (fetch-chapter-numbers conn "Himlit" "Revelation" "bo")
  ,)


(defn output-bo-markdown [source book]
  (let [header (get-text (select [ALL #(or (= (:type %) :h1) (and (= (:type %) :h5) (= (:lang %) "bo")))]
                                 (fetch-header conn "Himlit" "Revelation")))
        book-text (get-book-text source book "bo")]
    (flatten [header book-text])))



(defn output-eng-markdown [source book]
  (let [header (get-text (select [ALL #(= (:lang %) "english")]
                                 (fetch-header conn "Himlit" "Revelation")))
        book-text (get-book-text source book "english")]
    (flatten [header book-text])))

(comment
  (output-eng-markdown "WEB" "Revelation"),)

(defn output-diglot-markdown [source1 lang1 source2 lang2 book]
  (debug "ODM: " source1 lang1 source2 lang2 book)
  (let [header (get-text (interleave (fetch-header-lang conn "Himlit" book lang1) (fetch-header-lang conn "Himlit" book lang2)))
        book-text (get-book-interleaved source1 lang1 source2 lang2 book)]
    (flatten [header book-text])))

(defn output-parallel-markdown [source1 lang1 source2 lang2 book]
  (let [header (get-text (interleave (fetch-header-lang conn "Himlit" book lang1) (fetch-header-lang conn "Himlit" book lang2)))
        book-text (get-book-parallel source1 lang1 source2 lang2 book)]
    (flatten [header book-text])))

(defn output-boeng-markdown [book]
  (let [source "Himlit"
        lang1 "bo"
        lang2 "english"]
    (output-diglot-markdown source lang1 source lang2 book)))

(defn output-boweb-markdown [book]
  (let [source1 "Himlit"
        source2 "WEB"
        lang1 "bo"
        lang2 "english"]
    (output-diglot-markdown source1 lang1 source2 lang2 book)))


(comment
  (def a (fetch-header-lang conn "Himlit" "Revelation" "english"))
  (def b (fetch-header-lang conn "Himlit" "Revelation" "bo"))

  (output-boeng-markdown "Himlit" "english" "WEB" "english" "Revelation")
  (output-boeng-markdown "Himlit" "bo" "WEB" "english" "Revelation")
  (output-boeng-markdown "WEB" "english" "Himlit" "bo" "Revelation")

  (get-text (flatten (get-chapter-text-interleaved "Himlit" "bo" "WEB" "english" "Revelation" 1)))
  (get-book-interleaved "Himlit" "bo" "WEB" "english" "Revelation")
  (get-book-text "WEB" "Revelation" "english")
  (fetch-chapter conn "WEB" "Revelation" "english" 1)
  ())

(defn output-boeng-parallel [book]
  (let [source "Himlit"
        lang1 "bo"
        lang2 "english"]
    (output-parallel-markdown source lang1 source lang2 book)))


;; post processing
;pandoc -s .\ProcessJames.md -c .\resources\css\main.css --metadata title="James" -o ProcessJamesBo.html

;C:\Users\MartinRoberts\AppData\Local\Pandoc\pandoc -s "James.out.md" -A .\resources\html\footer.html -c .\resources\css\main.css    -o "James.html"

