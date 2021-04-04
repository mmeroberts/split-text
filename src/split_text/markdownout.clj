(ns split-text.markdownout
  (:require
    [split-text.config :as conf]
    [split-text.db :as db]
    [clojure.string :as str]
    [com.rpl.specter :refer [select transform ALL FIRST]]
    [split-text.wylie :as wy]))


(defn handle-bo-brackets [l]
  (let [l1 (str/replace l #"\\\*" "<span class=\"bo-ref\">*</span>")]
    (str/replace l1 #"(\[\d+\]|[\(\)\[\]\:]|\d+\:\d+(\-\d+)?|[0-9]+,[0-9]+|666|216|24|an ERV paraphrase|3:1-15)" conf/bo-brackets)))


(defn transform-underline-style-line [l]
  (str/replace l #"(\{(.+?)\})" split-text.config/name-highlight-style))

(defn wrap-verse-in-span
  "Wrap a line that starts with a verse number in a span with class of lang"
  [lang text]
  (if (str/starts-with? text "> ")
    text
    (str "<span class=\"v-" (name lang) "\">" text "</span>\n")))


(defn wrap-quote-in-span [lang text]
  (if (str/starts-with? text "> ")
    (str "> <span class=\"vq-" (name lang) "\">" (subs text 2) "</span>\n")
    text))


(defn handle-quotations [l]
  (if conf/ignore-quotations?
    (let [pass1 (str/join (select [:lines ALL :line] l))
          pass2 (str/replace pass1 #"^\> " "")]
      pass2)
    (let [wrapquote (partial wrap-quote-in-span (:lang l))]
      (transform [:lines ALL :line] wrapquote l))))

(defn handle-verse [l]
  (let [wrapverse (partial wrap-verse-in-span (:lang l))]
    (transform [:lines ALL :line] wrapverse l)))

(defn handle-slash-star [text]
  (str/replace text #"\\\\*" ""))
#_(comment
    (str/char-escape-string \\)
    (def text "sshsh\\*hshsh")
    (handle-slash-star text)
    ,)

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
  (cond (str/starts-with? text "> ") (str "<p class=\"pq-" lang "\">" (subs text 2) "</p>")
        (str/starts-with? text ">") (str "<p class=\"pq-" lang "\">" (subs text 1) "</p>")
        :else (str "<p class=\"p-" lang "\">" text "</p>")))

(defn join-lines [l]
  (let [wrap-divs (partial wrap-in-div (:lang l))
        pass1 (transform [:lines ALL :line] wrap-divs l)
        text (handle-slash-star (str/join (select [:lines ALL :line] pass1)))]
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
        text (handle-slash-star (first (select [:lines ALL :line] l)))]
    (assoc l :text-out (str "<" class " class=\"" class "-" (name lang) "\">" text "</" class ">\n"))))


(defn verse [l]
  (let [lang (:lang l)
        text (process-text l)]
    text))

(defn h4 [l]
  (verse l))




(defn prepare-markdown-text [style md]
  ;(debug "om: "(first md))
  (for [l md]
    (if (= (:type l) :verse)
      (verse l)
      (apply-class l (name (:type l))))))


#_(defn output-div-pairs [md]
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
        (conf/debug "A: " H1 H2 S1 S2)
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

(defn get-text [md]
  (let [text (prepare-markdown-text nil md)]
        ;x (debug "gt:" (first text))]
    (select [ALL :text-out] text)))

(defn order-source-text [items source_text]
  (for [item items]
    (select [ALL #(= (first (:verse-nos %)) (second item))] ((first item)  source_text))))



(defn get-chapter-text-interleaved [source-text1 source-text2]
  (let [source-text {:s1 (:verses source-text1)
                     :s2 (:verses source-text2)
                     :h1 (:headers source-text1)
                     :h2 (:headers source-text2)}
        items (interleave-chapter-items source-text)
        ordered_source_text (order-source-text items source-text)
        text (get-text (flatten ordered_source_text))]
    text))




(defn handle-parallel-text [ items source-text]
  (tap> source-text)
  (tap> items)
  (loop [[left right & more] items output ""]
    (conf/debug "[" left "][" right "][" more "]")
    (if (empty? left)
      output
      (let [left-text (select [ALL #(= (first (:verse-nos %)) (second left))] ((first left) source-text))
            right-text (select [ALL #(= (first (:verse-nos %)) (second right))] ((first right) source-text))
            left-output (first (get-text left-text))
            extra (first more)
            ;x (debug "row: " left-output)
            right-output (first (get-text right-text))
            real-right-text (if (= :s2 (first extra))
                              (let [extra-text  (select [ALL #(= (first (:verse-nos %)) (second extra))] ((first extra) source-text))
                                    extra-output (first (get-text extra-text))]
                                (str right-output extra-output))
                              right-output)
            real-more (if (= :s2 (first extra)) (rest more) more)
            div (str conf/row-div-open conf/column-div-open left-output conf/div-close conf/column-div-open real-right-text conf/div-close conf/div-close)]
        (recur real-more (str output div))))))

#_(comment
    (def items [[:s1 8]
                [:s2 8]
                [:s2 9]
                [:s1 10]
                [:s2 10]])
    (apply list items)
    (let [[left right  & more]  items]
      (debug left right  more)
      (tap> (rest more))
      (tap> (first more)))

    (rest items)
    ,)




(defn get-chapter-text-parallel [source-text1 source-text2]
  (let [source-text {:s1 (:verses source-text1)
                     :s2 (:verses source-text2)
                     :h1 (:headers source-text1)
                     :h2 (:headers source-text2)}
        items (interleave-chapter-items source-text)
        ordered_source_text (handle-parallel-text items source-text)]
    ordered_source_text))


(defn get-source-text [source book lang source-book  chapter]
  (let [source-text {:verses (select [ALL #(and (= (:chapter-no %) chapter) (= (:type %) :verse))] source-book)
                     :headers (select [ALL #(and (= (:chapter-no %) chapter) (= (:type %) :h3))] source-book)}]
    (if (empty? (:headers source-text))
      (let [eng-headers (db/fetch-chapter-verse-headings db/conn "Himlit" book lang chapter)
            alt-headers (if (seq eng-headers) eng-headers (db/fetch-chapter-verse-headings db/conn "Himlit" book "back" chapter))]
        (assoc source-text :headers  (vec alt-headers)))
      source-text)))


(defn get-chapter-header [source book lang ch source-book]
  (let [header (select [ALL  #(and (= (:chapter-no %) ch) (= (:type %) :h2))] source-book)]
    (if (seq header)
      header
      (let [new-header [{:index 1
                         :book book
                         :lines [{:line-no 1
                                  :line (str ch)}]
                         :type :h2
                         :source source
                         :verse-nos [1]
                         :lang lang
                         :chapter (str ch)
                         :chapter-no ch
                         :verse-number "1"}]]
        new-header))))

(defn handle-chapter [layout source1 lang1 source2 lang2 book source1-book source2-book ch]
  (let [header1 (get-text(get-chapter-header source1 book lang1 ch source1-book))
        header2 (get-text(get-chapter-header source2 book lang2 ch source2-book))
        source-text1 (get-source-text source1 book lang1 source1-book ch)
        source-text2 (get-source-text source2 book lang2 source2-book ch)
        chapter (if (= layout :parallel)
                  (get-chapter-text-parallel source-text1 source-text2)
                  (get-chapter-text-interleaved source-text1 source-text2))]
    [(conf/get-row-tiny-image (str "chapter-" ch)) header1 header2 chapter]))

(defn get-one-book [source book lang]
  (if (not= lang "wylie")
    (db/fetch-book db/conn source book lang)
    (wy/get-wylie-post-lines(db/fetch-book db/conn source book "bo"))))

(comment
  (wy/get-wylie-post-lines(db/fetch-book db/conn "Himlit" "Revelation" "bo"))
        ,)



(defn get-book [layout source1 lang1 source2 lang2 book]
  (let [source1-book (get-one-book source1 book lang1)
        source2-book (get-one-book source2 book lang2)
        chapters-in (sort(set(select [ALL :chapter-no] source1-book)))
        ref (:reference @conf/config-atom)
        chapters (conf/handle-reference chapters-in ref)
        text (for [ch chapters]
               (handle-chapter layout source1 lang1 source2 lang2 book source1-book source2-book ch))]

    text))

#_(comment
    (def source1-book (db/fetch-book db/conn "Himlit" "Revelation" "bo"))
    (def source2-book (db/fetch-book db/conn "BSB" "Revelation" "english"))
    (def source1-book (fetch-book conn "Himlit" "Mark" "bo"))
    (def source2-book (fetch-book conn "WEB" "Mark" "english"))
    (handle-chapter :interleaved "Himlit" "bo" "WEB" "english" "Mark" source1-book source2-book 1)
    (sort(set(select [ALL :chapter-no] source2-book)))
    (select [ALL  #(and (= (:chapter-no %) 1) (= (:type %) :h2))] source1-book)
    (get-text(get-chapter-header "WEB" "Revelation" "english" 1 source2-book))
    (def both-books (flatten [ source1-book source2-book]))
    (select [ALL  #(and (= (:chapter-no %) 2) (= (:type %) :h2)) :verse-number] both-books)
    (select [ALL #(and (= (:chapter-no %) 22) (= (:type %) :h3)) :verse-number] source2-book)
    (select [ALL #(and (= (:type %) :h3)) :verse-number] source2-book)
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
  (let [chapters-in (db/fetch-chapter-numbers db/conn source book language)
        ref (:reference @conf/config-atom)
        chapters (conf/handle-reference chapters-in ref)
        text (for [ch chapters]
               (let [header (get-text (db/fetch-chapter-header-lang db/conn "Himlit" book language ch))
                     chapter (get-text (db/fetch-full-chapter db/conn source book language ch))]
                 [(conf/get-row-tiny-image (str "chapter-" ch)) header chapter]))]
    text))



(defn output-uniglot-markdown [source lang book]
  (let [header (get-text (select [ALL #(= (:lang %) lang)]
                                 (db/fetch-header db/conn "Himlit" book)))
        book-text (get-book-text source book lang)]
    (flatten [header book-text])))

(comment
  (db/fetch-chapter-header-lang db/conn "Himlit" "Revelation" "bo" "2")
  (output-uniglot-markdown "Himlit" "bo" "Revelation")
  ,)




(defn output-markdown [format source1 lang1 source2 lang2 book]
  (conf/debug "ODM: " source1 lang1 source2 lang2 book)
  (let [header (get-text (sort-by :index (distinct(flatten (conj (db/fetch-header-lang db/conn source1 book lang1)
                                                        (when (and (= source1 "Himlit") (contains? #{ "bo" "wylie" } lang1))
                                                          (db/fetch-header-lang db/conn "Himlit" book "english"))
                                                        (db/fetch-header-lang db/conn source2 book lang2))))))
        book-text (get-book format source1 lang1 source2 lang2 book)]
    (flatten [header book-text])))

(comment
  (get-book "normal" "Himlit" "wylie" "Himlit" "english" "Revelation")
  (output-markdown "normal" "Himlit" "wylie" "Himlit" "english" "Revelation")
  (def source1 "Himlit")
  (def lang1 "wylie")
  (def source2 "Himlit")
  (def lang2 "english")
  (def book "Revelation")
  (get-text (sort-by :index (flatten (conj (db/fetch-header-lang db/conn source1 book lang1)
    (when (and (= source1 "Himlit") (contains? #{ "bo" "wylie" } lang1))
      (db/fetch-header-lang db/conn "Himlit" book "english"))))))
  (get-text (sort-by :index (distinct(flatten (conj (db/fetch-header-lang db/conn source1 book lang1)
                                           (when (and (= source1 "Himlit") (contains? #{ "bo" "wylie" } lang1)) (db/fetch-header-lang db/conn "Himlit" book "english"))
                                           (db/fetch-header-lang db/conn source2 book lang2))))))

  (db/fetch-header-lang db/conn source1 book lang1)
  (db/fetch-header-lang db/conn "Himlit" book "english")
  (db/fetch-header-lang db/conn source2 book lang2)
  ,)


(defn output-book [book source1 lang1 source2 lang2 format]
  (cond (nil? source2)    (output-uniglot-markdown source1 lang1 book) ; single language
        (= format "normal") (output-markdown :interleaved source1 lang1 source2 lang2 book)
        (= format "parallel") (output-markdown :parallel source1 lang1 source2 lang2 book)))



