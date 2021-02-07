(ns split-text.markdownout
  (:require
    [split-text.config :refer :all]
    [split-text.db :refer :all]
    [clojure.string :as str]
    [split-text.io :refer :all]
    [com.rpl.specter :refer :all]))


(defn filter-markdown-for-eng [l]
  (and (= (:lang l) :english) (contains? #{ :verse :h1 :h2 :h3 :h4 :h5} (:type l))))

(defn filter-markdown-for-bo [l]
  (or (and (= (:lang l) :bo) (contains? #{ :verse :h1 :h2 :h3 :h4 :h5} (:type l)))
      (and    (= (:lang l) :english) (contains? #{ :h2 :h1} (:type l)))))

(defn filter-markdown-for-boeng [l]
  (or (and (contains? #{:bo :english} (:lang l)) (contains? #{:verse :h1 :h2 :h3 :h4 :h5} (:type l)))  (= (:type l) :h1)))




(defn handle-bo-brackets [l]
  (str/replace l #"(\[\d+\]|[\(\)\[\]\:]|\d+\:\d+(\-\d+)?|[0-9]+,[0-9]+|666|216|an ERV paraphrase|\&apos;|\&quot;)" bo-brackets))

(defn transform-underline-style-line [l]
  (str/replace l #"(\{(.+?)\})" split-text.config/name-highlight-style))

(defn wrap-verse-in-span [lang text]
  "Wrap a line that starts with a verse number in a span with class of lang"
  (if (str/starts-with? text "> ")
    (str/replace text #"\> " "")
    (str "<span class=\"v-" (name lang) "\">" text "</span>\n")))


(defn wrap-quote-in-span [lang text]
  (if (str/starts-with? text "> ")
    (str "> <span class=\"vq-" (name lang) "\">" (subs text  2) "</span>\n")
    text))


(defn handle-quotations [l]
  (if ignore-quotations?
    (let [pass1 (str/join (select [:lines ALL :line] l))
          pass2 (str/replace pass1 #"\> " "")]
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
    (str "> " "<span class=\"vn\">" verse-number "</span>" (subs text 2))
    (str "<span class=\"vn\">" verse-number "</span>" text)))


(defn attach-verse-number [l]
  (let [min-lineno (apply min (select [:lines ALL :line-no ] l))
        add-vn (partial add-verse-number (:verse-number l))]
    (transform  [:lines ALL #(= (:line-no %) min-lineno) :line] add-vn l)))

(defn wrap-in-div [lang text]
  (str "<div class=\"p-" lang "\"><p>" text "</p></div>"))

(defn join-lines [l]
  (let [wrap-divs (partial wrap-in-div (:lang l))
        pass1 (transform [:lines ALL :line] wrap-divs l)
        text (str/join (select [:lines ALL :line] pass1))]
    (assoc l :text-out text)))


(defn process-text [l]
  (let [lang (:lang l)
        pass1 (if (= lang "bo") (transform [:lines ALL :line] process-bo-text l) l)]
    (-> pass1
        (attach-verse-number )
         (handle-quotations )
         (handle-verse)
         (join-lines))))

(defn apply-class [ l class ]
  (let [lang (:lang l)
        text (first(select [:lines ALL :line ] l))]
    (assoc l :text-out (str "<" class  " class=\"" class "-" (name lang) "\">" text "</" class">\n"))))

(defn h1 [_ l]
  (apply-class l "h1"))

(defn h2 [style l]
  (let [lang (:lang l)
        text (first (select [:lines ALL :line] l))
        textout (cond (and (= style :boeng-cols) (= lang ":english"))
              (str row-tiny-image "<div><h2 id=\"" (str/trim text) "\" class=\"h3-" (name lang) "\">" text "</h2>" "</div>\n") ;; h3-english works for two cols
              (and (or (= style :boeng) (= style :bo)) (= lang "bo"))
              (str row-tiny-image "<div><h2 id=\"" (str/trim text) "\" class=\"h2-" (name lang) "\">" text "</h2>" "</div>\n")
              :else (str "<div><h2 class=\"h2-" (name lang) "\">" text "</h2>\n"))]
    (assoc l :text-out textout)))

(defn h3 [_ l]
  (apply-class l "h3"))

(defn h5 [_ l]
  (apply-class l "h5"))



(defn verse [_ l]
  (let [lang (:lang l)
        text (process-text l)]
    text))

(defn h4 [style l]
  (verse style l)
  )

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