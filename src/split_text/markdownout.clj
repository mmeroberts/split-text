(ns split-text.markdownout
  (:require
    [split-text.config :as conf]
    [split-text.db :as db]
    [clojure.string :as str]
    [com.rpl.specter :refer :all]
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
  (process-text l))



(defn prepare-markdown-text [style md]
  ;(debug "om: "(first md))
  (for [l md]
    (if (= (:type l) :verse)
      (verse l)
      (apply-class l (name (:type l))))))



(defn get-text [md]
  (when (some? md)
    (let [text (prepare-markdown-text nil md)]
          ;x (debug "gt:" (first text))]
      (select [ALL :text-out] text))))


(defn match-verse-nos
  "match the verse number from their verso-nos vector"
  [ v1 v2 ]
  (not (nil? (some (set v1) v2))))

 
(defn find-matching-entry
  "Given and entry find an similar one in the entries nth instance"
  [entry entries idx]
  ;(prn "FME:" entry idx)
  (let [{:keys [type verse-nos chapter-no]}entry]
    (filter #(and (= (:type %) type) (match-verse-nos (:verse-nos %) verse-nos) (= (:chapter-no %) chapter-no)) (nth entries idx))))

(defn parallel-output
  "takes and entry and creates a row of text"
  [entries e num-of-sources]
  (let [text ( str conf/column-div-open (reduce str (get-text [e]) )conf/div-close)
        content (for [i (range 1 num-of-sources)]
                  (let [matching-entries (find-matching-entry e entries i)]
                    (if (nil? matching-entries)
                      (str conf/column-div-open conf/div-close)
                      (let [c (get-text  matching-entries)]
                        (str conf/column-div-open  (reduce str c) conf/div-close)))))
        o [(str conf/row-div-open text (reduce str content)  conf/div-close) ]]
    o))

(defn interleave-output
  "takes and entry and interleaves the text"
  [entries e num-of-sources]
  (let [text  (reduce str (get-text [e]) )
        content (for [i (range 1 num-of-sources)]
                  (let [matching-entries (find-matching-entry e entries i)]
                    (if (nil? matching-entries)
                      ""
                      (let [c (get-text  matching-entries)]
                        (reduce str c)))))
        o [(str  text (reduce str content) ) ]]
    o))



(defn get-chapter-text [entries num-of-sources output-fn]
    (loop [[e & r] (first entries) output []]
      (if (nil? e)
        output
        (let [o (output-fn entries e num-of-sources)]
          (recur r (conj output o ))))))



(defn get-chapter-headers [book ch entries]
  (let [header (select [ALL ALL #(and (= (:chapter-no %) ch) (= (:type %) :h2))] entries)]
    (if (seq header)
      header
      (let [new-header [{:index 1
                         :book book
                         :lines [{:line-no 1
                                  :line (str ch)}]
                         :type :h2
                         :source "Himlit"
                         :verse-nos [1]
                         :lang "english"
                         :chapter (str ch)
                         :chapter-no ch
                         :verse-number "1"}]]
        new-header))))

(defn handle-chapter [book layout-fn ch entries num-of-sources]
  (let [header (get-text(get-chapter-headers  book ch entries))
        verse-and-headings (for [b entries] (select [ALL  #(and (= (:chapter-no %) ch) (contains? #{:h3 :verse} (:type %)))] b))
        chapter (get-chapter-text verse-and-headings num-of-sources layout-fn)]
    [(conf/get-row-tiny-image (str "chapter-" ch)) header chapter]))



(defn get-book-entries [book sources]
  (for [source-in sources]
    (let [{:keys [source lang]} source-in]
      (db/fetch-book db/conn source book lang))))

(comment
  (def entries (get-book-entries "Revelation" [{:source "Himlit", :lang "bo"} {:source "Himlit", :lang "english"}]))
  (select [ALL ALL #(= (:chapter-no %) 1)] entries)
  ,)


(defn get-book [book layout-fn sources]
  (let [book-entries (get-book-entries book sources)
        chapters-in (sort (set (select [FIRST ALL :chapter-no] book-entries)))
        ref (:reference @conf/config-atom)
        chapters (conf/handle-reference chapters-in ref)
        text (for [ch chapters]
               (let [entries (for [b book-entries] (select [ALL #(= (:chapter-no %) ch)] b))]
                 (handle-chapter book layout-fn ch entries (count sources))))]
    text))

(defn get-header [book sources]
  (get-text
    (remove empty?
          (sort-by :index
                  (distinct
                    (flatten
                      (for [source-in sources]
                        (let [{:keys [ source lang]} source-in]
                          (conj (db/fetch-header-lang db/conn source book lang)
                            (when (and (= source "Himlit") (contains? #{ "bo" "wylie" } lang))
                              (db/fetch-header-lang db/conn "Himlit" book "english")))))))))))

#_(comment
  (def sources [{:source "Himlit" :lang "bo"} {:source "Himlit" :lang "english"} {:source "BSB" :lang "english"}])
  (def sources [{:source "Himlit" :lang "bo"} {:source "Himlit" :lang "english"} ])
  (def entries (get-book-entries "Revelation" sources))
  (handle-chapter "Revelation" :parallel 2 entries 2)
  (get-book "Revelation" :normal sources)
   (get-header "Revelation" [{:source "Himlit" :lang "bo"} {:source "Himlit" :lang "english"} {:source "BSB" :lang "english"}])
  (get-text(get-header "Revelation" [{:source "Himlit" :lang "bo"}]))

  ,)


(defn output-markdown [book layout-fn sources]
  (conf/debug "ODM: " sources book)
  (let [header (get-header  book sources)
        book-text (get-book book layout-fn sources)]
    (flatten [header book-text])))

#_(comment
  (def source1 [{:source "Himlit" :lang "bo"}])
  (get-book "Revelation" "normal" source1)
  (def boeng [{:source "Himlit", :lang "bo"} {:source "Himlit", :lang "english"}])
  (get-header "Revelation" boeng)
  ,)



(defn output-book [book format sources]  ; sources [ {:source source :lang} ...]
  (cond ;(= (count sources) 1)    (output-uniglot-markdown (first sources) book) ; single language
        (= format "normal") (output-markdown book interleave-output  sources)
        (= format "parallel") (output-markdown book parallel-output sources)))



#_(comment
  (def set1 [{:index 24,
              :book "Revelation",
              :lines [{:line-no 24,
                       :line "དེར་ག་རེ་ཡོད་པ་ཚང་མའི་བྱུང་རྐྱེན་དེ་ང་ཡིན་པ་དང་།&emsp;ཚང་མ་མཐའ་ཅན་བྱེད་མཁན་དེ་ང་ཡིན་ཟེར་ནས་ཀུན་དབང་གཙོ་བོ་དཀོན་མཆོག་གིས་གསུང་གི་ཡོད་རེད།&emsp;ང་ལ་དབང་ཆ་ཆ་ཚང་ཡོད།&emsp;ང་གསོན་པོ་ཡིན་པ་དང་།&emsp;ང་ག་དུས་ཡིན་ནས་གསོན་པོ་ཡིན།&emsp;ང་ཡོང་གི་ཡིན།"}],
              :type :verse,
              :source "Himlit",
              :verse-nos [1],
              :lang "bo",
              :chapter "1",
              :chapter-no 1,
              :verse-number "8"}
             {:index 25,
              :book "Revelation",
              :lines [{:line-no 25,
                       :line "\"I cause all things that there are to come into being and I cause the end of all things,\" says the Lord God Almighty. \"I have all authority. I am alive and I always was alive. And I will come.\""}],
              :type :verse,
              :source "Himlit",
              :verse-nos [1],
              :lang "english",
              :chapter "1",
              :chapter-no 1,
              :verse-number "8"}
             {:index 30707,
              :book "Revelation",
              :lines [{:line-no 1,
                       :line "On the Lord's day I was in the Spirit, and I heard behind me a loud voice like a trumpet,"}],
              :type :verse,
              :source "BSB",
              :verse-nos [1],
              :lang "english",
              :chapter "1",
              :chapter-no 1,
              :verse-number "10"}])
  (def set2 [{:index 24,
              :book "Revelation",
              :lines [{:line-no 24,
                       :line "དེར་ག་རེ་ཡོད་པ་ཚང་མའི་བྱུང་རྐྱེན་དེ་ང་ཡིན་པ་དང་།&emsp;ཚང་མ་མཐའ་ཅན་བྱེད་མཁན་དེ་ང་ཡིན་ཟེར་ནས་ཀུན་དབང་གཙོ་བོ་དཀོན་མཆོག་གིས་གསུང་གི་ཡོད་རེད།&emsp;ང་ལ་དབང་ཆ་ཆ་ཚང་ཡོད།&emsp;ང་གསོན་པོ་ཡིན་པ་དང་།&emsp;ང་ག་དུས་ཡིན་ནས་གསོན་པོ་ཡིན།&emsp;ང་ཡོང་གི་ཡིན།"}],
              :type :h3,
              :source "Himlit",
              :verse-nos [1],
              :lang "bo",
              :chapter "1",
              :chapter-no 1,
              :verse-number "8"}
             {:index 25,
              :book "Revelation",
              :lines [{:line-no 25,
                       :line "\"I cause all things that there are to come into being and I cause the end of all things,\" says the Lord God Almighty. \"I have all authority. I am alive and I always was alive. And I will come.\""}],
              :type :verse,
              :source "Himlit",
              :verse-nos [1],
              :lang "english",
              :chapter "1",
              :chapter-no 1,
              :verse-number "8"}
             {:index 30707,
              :book "Revelation",
              :lines [{:line-no 1,
                       :line "On the Lord's day I was in the Spirit, and I heard behind me a loud voice like a trumpet,"}],
              :type :verse,
              :source "BSB",
              :verse-nos [1],
              :lang "english",
              :chapter "1",
              :chapter-no 1,
              :verse-number "10"}])
  (def set3 [{:index 24,
              :book "Revelation",
              :lines [{:line-no 24,
                       :line "དེར་ག་རེ་ཡོད་པ་ཚང་མའི་བྱུང་རྐྱེན་དེ་ང་ཡིན་པ་དང་།&emsp;ཚང་མ་མཐའ་ཅན་བྱེད་མཁན་དེ་ང་ཡིན་ཟེར་ནས་ཀུན་དབང་གཙོ་བོ་དཀོན་མཆོག་གིས་གསུང་གི་ཡོད་རེད།&emsp;ང་ལ་དབང་ཆ་ཆ་ཚང་ཡོད།&emsp;ང་གསོན་པོ་ཡིན་པ་དང་།&emsp;ང་ག་དུས་ཡིན་ནས་གསོན་པོ་ཡིན།&emsp;ང་ཡོང་གི་ཡིན།"}],
              :type :h3,
              :source "Himlit",
              :verse-nos [2],
              :lang "bo",
              :chapter "1",
              :chapter-no 1,
              :verse-number "8"}
             {:index 25,
              :book "Revelation",
              :lines [{:line-no 25,
                       :line "\"I cause all things that there are to come into being and I cause the end of all things,\" says the Lord God Almighty. \"I have all authority. I am alive and I always was alive. And I will come.\""}],
              :type :verse,
              :source "Himlit",
              :verse-nos [1],
              :lang "english",
              :chapter "1",
              :chapter-no 1,
              :verse-number "8"}
             {:index 30707,
              :book "Revelation",
              :lines [{:line-no 1,
                       :line "On the Lord's day I was in the Spirit, and I heard behind me a loud voice like a trumpet,"}],
              :type :verse,
              :source "BSB",
              :verse-nos [1],
              :lang "english",
              :chapter "1",
              :chapter-no 1,
              :verse-number "10"}])
  (def set4 [{:index 24,
              :book "Revelation",
              :lines [{:line-no 24,
                       :line "དེར་ག་རེ་ཡོད་པ་ཚང་མའི་བྱུང་རྐྱེན་དེ་ང་ཡིན་པ་དང་།&emsp;ཚང་མ་མཐའ་ཅན་བྱེད་མཁན་དེ་ང་ཡིན་ཟེར་ནས་ཀུན་དབང་གཙོ་བོ་དཀོན་མཆོག་གིས་གསུང་གི་ཡོད་རེད།&emsp;ང་ལ་དབང་ཆ་ཆ་ཚང་ཡོད།&emsp;ང་གསོན་པོ་ཡིན་པ་དང་།&emsp;ང་ག་དུས་ཡིན་ནས་གསོན་པོ་ཡིན།&emsp;ང་ཡོང་གི་ཡིན།"}],
              :type :verse,
              :source "Himlit",
              :verse-nos [2],
              :lang "bo",
              :chapter "1",
              :chapter-no 1,
              :verse-number "8"}
             {:index 25,
              :book "Revelation",
              :lines [{:line-no 25,
                       :line "\"I cause all things that there are to come into being and I cause the end of all things,\" says the Lord God Almighty. \"I have all authority. I am alive and I always was alive. And I will come.\""}],
              :type :h3,
              :source "Himlit",
              :verse-nos [1],
              :lang "english",
              :chapter "1",
              :chapter-no 1,
              :verse-number "8"}
             {:index 30707,
              :book "Revelation",
              :lines [{:line-no 1,
                       :line "On the Lord's day I was in the Spirit, and I heard behind me a loud voice like a trumpet,"}],
              :type :verse,
              :source "BSB",
              :verse-nos [2],
              :lang "english",
              :chapter "1",
              :chapter-no 1,
              :verse-number "10"}])
  (def set5 [{:index 24,
              :book "Revelation",
              :lines [{:line-no 24,
                       :line "དེར་ག་རེ་ཡོད་པ་ཚང་མའི་བྱུང་རྐྱེན་དེ་ང་ཡིན་པ་དང་།&emsp;ཚང་མ་མཐའ་ཅན་བྱེད་མཁན་དེ་ང་ཡིན་ཟེར་ནས་ཀུན་དབང་གཙོ་བོ་དཀོན་མཆོག་གིས་གསུང་གི་ཡོད་རེད།&emsp;ང་ལ་དབང་ཆ་ཆ་ཚང་ཡོད།&emsp;ང་གསོན་པོ་ཡིན་པ་དང་།&emsp;ང་ག་དུས་ཡིན་ནས་གསོན་པོ་ཡིན།&emsp;ང་ཡོང་གི་ཡིན།"}],
              :type :verse,
              :source "Himlit",
              :verse-nos [5],
              :lang "bo",
              :chapter "1",
              :chapter-no 1,
              :verse-number "8"}
             {:index 25,
              :book "Revelation",
              :lines [{:line-no 25,
                       :line "\"I cause all things that there are to come into being and I cause the end of all things,\" says the Lord God Almighty. \"I have all authority. I am alive and I always was alive. And I will come.\""}],
              :type :h3,
              :source "Himlit",
              :verse-nos [5],
              :lang "english",
              :chapter "1",
              :chapter-no 1,
              :verse-number "8"}
             {:index 30707,
              :book "Revelation",
              :lines [{:line-no 1,
                       :line "On the Lord's day I was in the Spirit, and I heard behind me a loud voice like a trumpet,"}],
              :type :verse,
              :source "BSB",
              :verse-nos [2],
              :lang "english",
              :chapter "1",
              :chapter-no 1,
              :verse-number "10"}])
  (def set6 [()
             {:index 25,
              :book "Revelation",
              :lines [{:line-no 25,
                       :line "\"I cause all things that there are to come into being and I cause the end of all things,\" says the Lord God Almighty. \"I have all authority. I am alive and I always was alive. And I will come.\""}],
              :type :h3,
              :source "Himlit",
              :verse-nos [2],
              :lang "english",
              :chapter "1",
              :chapter-no 1,
              :verse-number "8"}
             {:index 30707,
              :book "Revelation",
              :lines [{:line-no 1,
                       :line "On the Lord's day I was in the Spirit, and I heard behind me a loud voice like a trumpet,"}],
              :type :verse,
              :source "BSB",
              :verse-nos [2],
              :lang "english",
              :chapter "1",
              :chapter-no 1,
              :verse-number "10"}])
  (choose-item set1) ; item 0 chosen - lowest verse
  (choose-item set2) ; item 0 h3
  (choose-item set3) ; item 1 because h3 has higher verse
  (choose-item set4) ; item 1 because h3 has lowest verse
  (choose-item set5) ; item 2 because h3 has lowest verse
  (choose-item set6) ; item 1 0 is empty
  (remove-entry set5 1)
  ,)