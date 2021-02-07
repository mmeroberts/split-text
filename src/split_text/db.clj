(ns split-text.db
  (:require    [clojure.string :as str]
               [clojure.java.io :as io]
               [crux.api :as crux]
               [byte-streams :as bs]
               [byte-transforms :as bt]
               [clojure.set :as set]

               [com.rpl.specter :refer :all]))




(defn start-http-client [port]
 (crux/new-api-client (str "http://localhost:" port)))

(def conn (start-http-client 3000))


;(defn add_entries [conn md book source]
;  (for [e md]
;    (let [initial-entry (select-keys e [:lang :type :index :verse-number :chapter :next-verse])
;          line-ids (vec (for [i (range (count (:fulltext e)))] (d/tempid -1)))
;          linesvec (vec (:fulltext e))
;          y (println line-ids "," linesvec)
;          lines (into [] (for [i (range (count (:fulltext e)))]
;                          (let [id (nth line-ids i)
;                                line (nth linesvec i)
;                                lineno (first line)
;                                text (last line)]
;                            (assoc {} :db/id id :text/line-no lineno :text/line text))))
;          entry (conj  {:db/id entry_id :book book :source source :lines line-ids} initial-entry)
;          transaction (conj lines entry)]
;      (d/transact conn transaction))))

(defn encode [l]
  (bs/to-string (bt/encode l :base64 {:url-safe? true})))

(defn decode [l]
  (bs/to-string (bt/decode l :base64 {:url-safe? true})))

(defn String->Number [str]
  (let [n (Integer/parseInt  str)]
    (if (number? n) n nil)))

(defn make-entry [source book  l]
  (let [index (:index l)
        bo (= (:lang l) :bo)
        lang (:lang l)
        id (keyword source (str book "_" (name lang) "_" index))
        type (:type l)
        chapter (:chapter l)
        chapter-no (String->Number chapter)
        verse-number (cond (contains? l :next-verse) (:next-verse l)
                           (= type :h2) "1"
                           (nil? (:verse-number l)) "0"
                           :else (:verse-number l))
        verse-nos (if (not= verse-number "")
                      (let [x (mapv String->Number (str/split verse-number #"\-+"))]
                           (vec (range (first x) (inc (last x)))))
                      nil)
        linesvec (vec (:fulltext l))
        lines (into [] (for [i (range (count (:fulltext l)))]
                         (let [line (nth linesvec i)
                               lineno (first line)
                               text (last line)]
                           (assoc {} :line-no lineno :line (if bo (encode text) text)))))]
    (assoc {} :crux.db/id id
           :index index :source source :type type :lang (name lang) :chapter-no chapter-no :verse-nos verse-nos
           :book book :chapter chapter :verse-number verse-number
           :lines  lines)))

(defn add_entries [conn source book md]
  (let [firstpass (map #(make-entry source book %) md)
        secondpass (map #(conj [] :crux.tx/put %) firstpass)]
    ;(println secondpass)
     (crux/submit-tx conn (vec secondpass))))

(defn fetch [conn query source book language chapter verse]
  (let [ results  (crux/q
                    (crux/db conn)
                    query)
        decoded (if (= language "bo")
                  (transform  [ALL ALL :lines ALL :line] decode results)
                  results)
        ;response (if (= chapter "1") (filter (fn [x] (not= (:verse-number x) 0) decoded)) decoded)
        ]
    (sort-by :index (select [ALL ALL] decoded))
    ))

(defn fetch-chapter [conn source book language chapter]
  (let [query {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number :chapter-no  :verse-nos  :type :lang :verse-nos :lines])]
               :where '[[?entry :chapter ?chpt]
                        [?entry :lang ?lang]
                        [?entry :book ?book]
                        [?entry :source ?source]
                        [?entry :verse-number ?verse]
                        [(not= ?verse "0")]]
               :args [{'?chpt chapter '?lang language '?book book '?source source}]}]
    (fetch conn query source book language chapter nil)))

(defn fetch-header [conn source book language ]
  (let [chapter 1
        verse 0

        query  {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number :chapter-no  :verse-nos :type  :lang :lines])]
                :where '[[?entry :chapter-no ?chpt]
                         [?entry :lang ?lang]
                         [?entry :book ?book]
                         [?entry :source ?source]
                         [?entry :verse-nos ?verse]]
                :args [{'?chpt chapter '?verse verse  '?lang language '?book book '?source source}]}]
    (fetch conn query source book language chapter verse)))

(defn fetch-verse [conn source book language chapter verse]
  (let [query  {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number :chapter-no  :verse-nos :type  :lang :lines])]
                :where '[[?entry :chapter-no ?chpt]
                         [?entry :lang ?lang]
                         [?entry :book ?book]
                         [?entry :source ?source]
                         [?entry :verse-nos ?verse]]
                :args [{'?chpt chapter '?verse verse  '?lang language '?book book '?source source}]}]
    (fetch conn query source book language chapter verse)))

(defn fetch-book [conn source book language]
  (let [query {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number  :chapter-no  :verse-nos :type :lang :lines])]
               :where '[[?entry :chapter ?chpt]
                        [?entry :lang ?lang]
                        [?entry :book ?book]
                        [?entry :source ?source]]
               :args [{'?lang language '?book book '?source source}]}]
    (fetch conn query source book language nil nil)))

(defn fetch-book-no-header [conn source book language]
    (let [full-book (fetch-book conn  source book language)
          header (fetch-header conn  source book language)]
      (sort-by :index (set/difference (set full-book) (set header)))))

(defn fetch-chapter-headings [conn source book language chapter]
  (let [query  {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number :chapter-no  :verse-nos :type :lang :lines])]
                :where '[[?entry :chapter ?chpt]
                         [?entry :lang ?lang]
                         [?entry :book ?book]
                         [?entry :source ?source]
                         [?entry :type :h3]]
                :args [{'?chpt chapter  '?lang language '?book book '?source source}]}]
    (fetch conn query source book language chapter nil)))



(comment
  (def entry [:crux.tx/put
              {:crux.db/id :web/Revelation_0,
               :index 0,
               :source "Himlit",
               :lang :english,
               :book "Revelation",
               :chapter "1",
               :verse-number nil,
               :lines [#:text{:line-no 0, :line " མངོན་པ།"}]}])
  (crux/submit-tx conn (conj [] entry))
  (crux/q
    (crux/db conn)
    '{:find [n]
      :where [[p1 :chapter n]]})


(def chpt "1")
  (def book "Revelation")
  (def source "Himlit")
  (def v "7")
  (def q {:find ['?p1 ]
          :where [['?p1 :chapter chpt]]})
  (crux/q
    (crux/db conn)
    '{:find [?p1 ]
      :where [[?p1 :chapter "15"]]})

  (crux/q (crux/db conn)
          {:find '[?e]
           :where '[[?e :chapter ?code]]
           :args [{'?code chpt}]})

(def x (crux/q
    (crux/db conn)
    {:find '[(eql/project ?entry [:index :chapter :verse-number :lines])]
      :where '[[?entry :chapter ?chpt]
              [?entry :lang "english"]
               [?entry :verse-number "9"]]
      :args [{'?chpt chpt}]}))

  (def y (crux/q
           (crux/db conn)
           {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number :lines])]
            :where '[[?entry :chapter ?chpt]
                     [?entry :lang "english"]
                     [?entry :book ?book]
                     [?entry :source ?source]
                     [?entry :verse-nos [24]]]
            :args [{'?chpt "2" '?book "Revelation" '?source "Himlit"}]}))

  (crux/q
    (crux/db conn)
    {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number :lines])]
     :where '[[?entry :chapter ?chpt]
              [?entry :lang "english"]
              [?entry :book ?book]
              [?entry :source ?source]
              [?entry :verse-nos 25]]
     :args [{'?chpt "2" '?book "Revelation" '?source "Himlit"}]})

  (transform  [ALL ALL :lines ALL :line] decode split-text.db/y)

  (fetch-chapter conn "Himlit" "Revelation" "english" "1")
  (fetch-chapter conn "Himlit" "Revelation" "bo" "5")
  (fetch-chapter conn "Himlit" "Revelation" "english" "2")
  (fetch-chapter conn "WEB" "Revelation" "english" "7")
  (fetch-header conn "Himlit" "Revelation" "english" )
  (fetch-book conn "Himlit" "Revelation" "english" )
  (fetch-book-no-header conn "Himlit" "Revelation" "english" )
  (fetch-chapter-headings conn "Himlit" "Revelation" "english" "1")
  (fetch-verse conn "Himlit" "Revelation" "english" 2 18)
  (fetch-verse conn "Himlit" "Revelation" "english" 5 9)
  (fetch-verse conn "Himlit" "Revelation" "bo" 5 9)
  (fetch-verse conn "Himlit" "Revelation" "english" 7 5)
  (fetch-verse conn "Himlit" "Revelation" "english" 7 6)

  (def bo1 (fetch-chapter conn "Himlit" "Revelation" "bo" "1"))
  (def eng1 (fetch-chapter conn "Himlit" "Revelation" "english" "1"))
  (def engheadings (fetch-chapter-headings conn "Himlit" "Revelation" "english" "1"))
  (def eng2 (fetch-chapter conn "WEB" "Revelation" "english" "1"))
  (sort-by (juxt :chapter :verse-number) (conj bo1 eng1))
  (sort-by (juxt :chapter :verse-number :type :lang ) (flatten (conj bo1 eng2 engheadings)))
  (flatten (conj bo1 eng2 engheadings))
  (crux/entity (crux/db conn) :Himlit/Revelation_english_642)
  (crux/entity (crux/db conn) :web/Revelation_30846)

  (crux/q
    (crux/db conn)
    '{:find [l]
      :where [[p1 :lines l]]})

  (decode (:line (first(first(crux/q
                               (crux/db conn)
                               '{:find [l]
                                 :where [[p1 :lines l]]})))))
  ,)



  ;
  ;










;;;;;;;; DB;;;;;;;;

