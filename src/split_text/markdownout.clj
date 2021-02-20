(ns split-text.markdownout
  (:require
    [split-text.config :refer :all]
    [split-text.db :refer :all]
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

(defn h1 [l]
  (apply-class l "h1"))

(defn h2 [l]
  (apply-class l "h2"))

(defn h3 [l]
  (apply-class l "h3"))

(defn h5 [l]
  (apply-class l "h5"))



(defn verse [l]
  (let [lang (:lang l)
        text (process-text l)]
    text))

(defn h4 [l]
  (verse l))


(defn prepare-output [md]
  (for [l md]
    (if (= (:type l) :verse)
      (verse l)
      (apply-class l (name (:type l))))))


(defn output-markdown [style md]
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



(defn interleave-chapter-text [source1-vn lang1 source2-vn lang2 headings1 heading2 source1 source2 book chapter]
  (loop [s1 source1-vn s2 source2-vn h1 headings1 h2 heading2 output []]
    (let [f (fn [x] (if (empty? x) 999 (first x)))
          S1 (f s1)
          S2 (f s2)
          H1 (f h1)
          H2 (f h2)]
      (debug "A: " H1 H2 S1 S2)
      (let [])
      (cond (and (empty? s1) (empty? s2))
            ;; exit loop
            output
            (<= H1 H2 S1 S2)
            (let [x (debug "3: " s1 s2 h1 h2)
                  t (fetch-verse-heading conn "Himlit" book lang1 chapter (first h1))]
              (recur s1 s2 (rest h1) h2 (conj output t)))
            (<= H2 S1 S2)
            (let [x (debug "4: " s1 s2 h1 h2)
                  t (fetch-verse-heading conn "Himlit" book lang2 chapter (first h2))]
              (recur s1 s2 h1 (rest h2) (conj output t)))
            (<= S1 S2)
            (let [x (debug "1: " (first s1) (first s2) (first h1) (first h2))
                  t (fetch-verse-by-nos conn source1 book lang1 chapter (first s1))]
              (recur (rest s1) s2 h1 h2 (conj output t)))
            :else
            (let [x (debug "2: " s1 s2 h1 h2)
                  t (fetch-verse-by-nos conn source2 book lang2 chapter (first s2))]
              (recur s1 (rest s2) h1 h2 (conj output t)))))))



(comment
  (fetch-verse-headings-verse-nos conn "Himlit" "Revelation" "bo" 2)
  (fetch-verse-nos conn "Himlit" "Revelation" "english" "2")
  (interleave-chapter-text '(1 2 3) "english" '(1 2 3) "bo" '(1) '(1) "WEB" "Himlit" "Revelation" 2)
  (> (first nil) (first [1]))
  (> nil 1)
  (if  1 2)
  (<= 1 3 4 5)
  ,)

(defn get-text [md]
  (select [ALL :text-out] (output-markdown nil md)))


(defn get-chapter-text-interleaved [source1 lang1 source2 lang2 book chapter]
  (let [source1-vn (fetch-verse-nos conn source1 book lang1 chapter)
        source2-vn (fetch-verse-nos conn source2 book lang2 chapter)
        heading-numbers1 (fetch-verse-headings-verse-nos conn "Himlit" book lang1 chapter)
        heading-numbers2 (fetch-verse-headings-verse-nos conn "Himlit" book lang2 chapter)
        text (get-text (flatten (interleave-chapter-text source1-vn lang1 source2-vn lang2 heading-numbers1 heading-numbers2 source1 source2 book chapter)))]
    text))

(defn get-book-interleaved [source1 lang1 source2 lang2 book]
  (let [chapters (fetch-chapter-numbers conn source1 book lang1)
        text (for [ch chapters]
               (let [header (get-text (fetch-chapter-header conn "Himlit" book ch))
                     chapter (get-chapter-text-interleaved source1 lang1 source2 lang2 book ch)]
                 [row-tiny-image header chapter]))]
    text))




(defn get-book-text [source book language]
  (let [chapters (fetch-chapter-numbers conn source book language)
        text (for [ch chapters]
               (let [header (get-text (fetch-chapter-header-lang conn "Himlit" book language ch))
                     chapter (get-text (fetch-chapter conn source book language ch))]
                 [row-tiny-image header chapter]))]
    text))


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
  (let [header (get-text (interleave (fetch-header-lang conn "Himlit" book lang1) (fetch-header-lang conn "Himlit" book lang2)))
        book-text (get-book-interleaved source1 lang1 source2 lang2 book)]
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

(defn output-boeng-interlinear [md]
  (output-div-pairs (filter filter-markdown-for-boeng md)))


;; post processing
;pandoc -s .\ProcessJames.md -c .\resources\css\main.css --metadata title="James" -o ProcessJamesBo.html

;C:\Users\MartinRoberts\AppData\Local\Pandoc\pandoc -s "James.out.md" -A .\resources\html\footer.html -c .\resources\css\main.css    -o "James.html"

(comment
  (prepare-output (fetch-verse conn "Himlit" "Revelation" "bo" 7 6))
  (prepare-output (fetch-header conn "Himlit" "Revelation" "english")),)