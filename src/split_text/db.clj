(ns split-text.db
  (:require    [split-text.config :as conf]
               [clojure.string :as str]
               [crux.api :as crux]
               [byte-streams :as bs]
               [byte-transforms :as bt]
               [clojure.set :as set]
               [clj-http.client :as cl]
               [clojure.edn :as edn]
               [com.rpl.specter :refer [select transform ALL]]))

(defn start-http-client [port]
 (crux/new-api-client (str "http://192.168.1.73:" port)))

(def conn (start-http-client 3000))


(defn encode [l]
  (bs/to-string (bt/encode l :base64 {:url-safe? true})))

(defn decode [l]
  (bs/to-string (bt/decode l :base64 {:url-safe? true})))

(defn String->Number [str]
  (let [n (Integer/parseInt  str)]
    (if (number? n) n nil)))




(defn make-entry [source book  l]
  (conf/debug l)
  (let [index (:index l)
        bo (= (:lang l) :bo)
        lang (:lang l)
        id (keyword source (str book "_" (name lang) "_" index))
        type (if (= (:type l) :h4) :verse (:type l))
        chapter (re-find #"\d+" (:chapter  l))
        chapter-no (String->Number (re-find #"\d+" chapter))
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
              :index index
              :source source
              :type type
              :lang (name lang)
              :chapter-no chapter-no
              :verse-nos verse-nos
              :book book
              :chapter chapter
              :verse-number verse-number
              :lines  lines)))

(comment
  (re-find #"\d+" "**9**")
  ,)

;(defn add_entries [conn source book md]
;  (let [firstpass (map #(make-entry source book %) md)
;        secondpass (map #(conj [] :crux.tx/put %) firstpass)]
;    (tap> secondpass)
;     (crux/submit-tx conn (vec secondpass))))

(defn add_entries [conn source book md]
    (for [entry md]
      (let [put [[:crux.tx/put entry]]
            y (tap> entry)
            x (tap> put)]
        (crux/submit-tx conn (vec put)))))

(defn fetch-by-connection [conn query]
  (let [ results  (crux/q
                    (crux/db conn)
                    query)
        decoded  (transform  [ALL ALL #(= (:lang %) "bo") :lines ALL :line] decode results)]
        ;response (if (= chapter "1") (filter (fn [x] (not= (:verse-number x) 0) decoded)) decoded)

    (sort-by :index (select [ALL ALL] decoded))))

(defn fetch-by-http [conn query]
  (let [resp (cl/post conn
                      {
                       :body               (str {:query query})
                       :headers            {"X-Api-Version" "2"}
                       :content-type       :edn
                       :socket-timeout     1000             ;; in milliseconds
                       :connection-timeout 1000             ;; in milliseconds
                       :accept             :edn
                       :as :edn})
        x (println (:body resp))
        results (edn/read-string (:body resp))
        decoded  (transform  [ALL ALL #(= (:lang %) "bo") :lines ALL :line] decode results)]
    (sort-by :index (select [ALL ALL] decoded))))

(defn fetch [conn query]
  (if (= (class conn) crux.remote_api_client.RemoteApiClient)
    (fetch-by-connection conn query)
    (fetch-by-http conn query)))

(defn fetch-book [conn source book language]
  (let [query {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number  :chapter-no  :verse-nos :type :lang :lines])]
               :where '[[?entry :chapter ?chpt]
                        [?entry :lang ?lang]
                        [?entry :book ?book]
                        [?entry :source ?source]]
               :args [{'?lang language '?book book '?source source}]}]
    (fetch conn query)))

(defn fetch-header-lang [conn source book language]
  (let [chapter 1
        verse 0
        query  {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number :chapter-no  :verse-nos :type  :lang :lines])]
                :where '[[?entry :chapter-no ?chpt]
                         [?entry :lang ?lang]
                         [?entry :book ?book]
                         [?entry :source ?source]
                         [?entry :verse-nos ?verse]]
                :args [{'?chpt chapter '?verse verse  '?lang language '?book book '?source source}]}]
    (fetch conn query)))


;;;;;;;;;; not used ;;;;;;;;;;

(defn fetch-full-chapter [conn source book language chapter]
  (let [chapter-in (if (string? chapter) chapter (str chapter))
        query {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number :chapter-no  :verse-nos  :type :lang :verse-nos :lines])]
               :where '[[?entry :chapter ?chpt]
                        [?entry :lang ?lang]
                        [?entry :book ?book]
                        [?entry :source ?source]
                        [?entry :type ?type]
                        [?entry :verse-number ?verse]
                        [(not= ?verse "0")]
                        [(not= ?type :h2)]]
               :args [{'?chpt chapter-in '?lang language '?book book '?source source}]}]
    (fetch conn query)))

(defn fetch-chapter [conn source book language chapter]
  (let [chapter-in (if (string? chapter) chapter (str chapter))
        query {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number :chapter-no  :verse-nos  :type :lang :verse-nos :lines])]
               :where '[[?entry :chapter ?chpt]
                        [?entry :lang ?lang]
                        [?entry :book ?book]
                        [?entry :source ?source]
                        [?entry :verse-number ?verse]
                        [?entry :type :verse]
                        [(not= ?verse "0")]]
               :args [{'?chpt chapter-in '?lang language '?book book '?source source}]}]
    (fetch conn query)))

(defn fetch-chapter-header-lang [conn source book language chapter]
  (let [chapter-in (if (string? chapter) chapter (str chapter))
        args (if (= language "bo") [{'?chpt chapter-in  '?book book '?source source}]
                                   [{'?chpt chapter-in '?lang language '?book book '?source source}])
        query {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number :chapter-no  :verse-nos  :type :lang :verse-nos :lines])]
               :where '[[?entry :chapter ?chpt]
                        [?entry :lang ?lang]
                        [?entry :book ?book]
                        [?entry :source ?source]
                        [?entry :type :h2]]
               :args args}]
    (fetch conn query)))

(defn fetch-chapter-header [conn source book  chapter]
  (let [chapter-in (if (string? chapter) chapter (str chapter))
        args [{'?chpt chapter-in  '?book book '?source source}]
        query {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number :chapter-no  :verse-nos  :type :lang :verse-nos :lines])]
               :where '[[?entry :chapter ?chpt]
                        [?entry :lang ?lang]
                        [?entry :book ?book]
                        [?entry :source ?source]
                        [?entry :type :h2]]
               :args args}]
    (fetch conn query)))






(defn fetch-header [conn source book]
  (let [chapter 1
        verse 0

        query  {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number :chapter-no  :verse-nos :type  :lang :lines])]
                :where '[[?entry :chapter-no ?chpt]
                         [?entry :book ?book]
                         [?entry :source ?source]
                         [?entry :verse-nos ?verse]]
                :args [{'?chpt chapter '?verse verse  '?book book '?source source}]}]
    (fetch conn query)))

(defn verse? [type]
  (or  (= type :verse) (= type :h4)))

(defn fetch-verse-by-number
  "by-number means using the :verse-number entry which is a string and can be of the format 18-19"
  [conn source book language chapter verse]
  (let [chapter-in (if (string? chapter) chapter (str chapter))
        verse-in (if (string? verse) verse (str verse))
        query  {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number :chapter-no  :verse-nos :type  :lang :lines])]
                :where '[[?entry :chapter ?chpt]
                         [?entry :lang ?lang]
                         [?entry :book ?book]
                         [?entry :source ?source]
                         [?entry :verse-number ?verse]
                         [?entry :type ?type]
                         [(= ?type :verse)]]
                :args [{'?chpt chapter-in '?verse verse-in  '?lang language '?book book '?source source}]}]
    (fetch conn query)))

(defn fetch-verse-by-nos [conn source book language chapter verse]
  (let [chapter-in (if (string? chapter) (String->Number chapter) chapter)
        verse-in (if (string? verse) (String->Number verse) verse)
        query  {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number :chapter-no  :verse-nos :type  :lang :lines])]
                :where '[[?entry :chapter-no ?chpt]
                         [?entry :lang ?lang]
                         [?entry :book ?book]
                         [?entry :source ?source]
                         [?entry :verse-nos ?verse]
                         [?entry :type ?type]
                         [(= ?type :verse)]]
                :args [{'?chpt chapter-in '?verse verse-in  '?lang language '?book book '?source source}]}]
    (fetch conn query)))



(defn fetch-book-verses [conn source book language]
  (let [query {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number  :chapter-no  :verse-nos :type :lang :lines])]
               :where '[[?entry :chapter ?chpt]
                        [?entry :lang ?lang]
                        [?entry :book ?book]
                        [?entry :source ?source]
                        [?entry :type :verse]]
               :args [{'?lang language '?book book '?source source}]}]
    (fetch conn query)))

(defn fetch-book-no-header [conn source book language]
    (let [full-book (fetch-book conn  source book language)
          header (fetch-header-lang conn  source book language)]
      (sort-by :index (set/difference (set full-book) (set header)))))

(defn fetch-verse-heading [conn source book language chapter verse]
  (let [chapter-in (if (string? chapter) chapter (str chapter))
        verse-in (if (string? verse) verse (str verse))
        query  {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number :chapter-no  :verse-nos :type :lang :lines])]
                :where '[[?entry :chapter ?chpt]
                         [?entry :lang ?lang]
                         [?entry :book ?book]
                         [?entry :source ?source]
                         [?entry :verse-number ?verse]
                         [?entry :type :h3]]
                :args [{'?chpt chapter-in  '?lang language '?book book '?source source '?verse verse-in}]}]
    (fetch conn query)))

(defn fetch-verse-headings [conn source book  chapter verse]
  (let [chapter-in (if (string? chapter) chapter (str chapter))
        verse-in (if (string? verse) verse (str verse))
        query  {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number :chapter-no  :verse-nos :type :lang :lines])]
                :where '[[?entry :chapter ?chpt]
                         [?entry :lang ?lang]
                         [?entry :book ?book]
                         [?entry :source ?source]
                         [?entry :verse-number ?verse]
                         [?entry :type :h3]]
                :args [{'?chpt chapter-in   '?book book '?source source '?verse verse-in}]}]
    (fetch conn query)))

(defn fetch-chapter-verse-headings [conn source book language chapter]
  (let [chapter-in (if (string? chapter) chapter (str chapter))
        query  {:find '[(eql/project ?entry [:index :book :source :chapter :verse-number :chapter-no  :verse-nos :type :lang :lines])]
                :where '[[?entry :chapter ?chpt]
                         [?entry :lang ?lang]
                         [?entry :book ?book]
                         [?entry :source ?source]
                         [?entry :type :h3]]
                :args [{'?chpt chapter-in  '?lang language '?book book '?source source}]}]
    (fetch conn query)))

(defn fetch-chapter-numbers [conn source book language]
  (let [results   (crux/q
                    (crux/db conn)
                    {:find '[?chpt]
                     :where '[[?entry :chapter-no ?chpt]
                              [?entry :lang ?lang]
                              [?entry :book ?book]
                              [?entry :source ?source]]
                     :args [{ '?lang language '?book book '?source source}]})]
    (flatten (sort results))))

(defn fetch-verse-numbers [conn source book language chapter]
  (let [chapter-in (if (string? chapter) chapter (str chapter))
        results   (crux/q
                    (crux/db conn)
                    {:find '[?verse-no ?verse-number]
                     :where '[[?entry :lang ?lang]
                              [?entry :book ?book]
                              [?entry :source ?source]
                              [?entry :chapter ?chapter]
                              [?entry :verse-number ?verse-number]
                              [?entry :verse-nos ?verse-no]]
                     :args [{ '?lang language '?book book '?source source '?chapter chapter-in}]})]
    (sort-by #(String->Number (first(str/split % #"\-+"))) (set(map second results)))))

(defn fetch-verse-nos [conn source book language chapter]
  (let [chapter-in (if (string? chapter) chapter (str chapter))
        results   (crux/q
                    (crux/db conn)
                    {:find '[?verse-no ?verse-number]
                     :where '[[?entry :lang ?lang]
                              [?entry :book ?book]
                              [?entry :source ?source]
                              [?entry :chapter ?chapter]
                              [?entry :verse-number ?verse-number]
                              [?entry :verse-nos ?verse-no]]
                     :args [{ '?lang language '?book book '?source source '?chapter chapter-in}]})]
    (sort(mapv #(String->Number (first(str/split % #"\-+"))) (set(map second results))))))

(defn fetch-verse-headings-verse-nos [conn source book language chapter]
  (let [chapter-in (if (string? chapter) chapter (str chapter))
        results   (crux/q
                    (crux/db conn)
                    {:find '[?verse-no ?verse-number]
                     :where '[[?entry :lang ?lang]
                              [?entry :book ?book]
                              [?entry :source ?source]
                              [?entry :chapter ?chapter]
                              [?entry :verse-number ?verse-number]
                              [?entry :verse-nos ?verse-no]
                              [?entry :type :h3]]
                     :args [{ '?lang language '?book book '?source source '?chapter chapter-in}]})]
    (sort(mapv #(String->Number (first(str/split % #"\-+"))) (set(map second results))))))

(defn fetch-books [conn source]
  (let [
        results   (crux/q
                    (crux/db conn)
                    {:find '[?book ?lang]
                     :where '[[?entry :lang ?lang]
                              [?entry :book ?book]
                              [?entry :source ?source]]
                     :args [{ '?source source}]})]
    results))

(defn fetch-all-books-by-lang [conn lang]
  (let [
        results   (crux/q
                    (crux/db conn)
                    {:find '[?source ?book ?lang]
                     :where '[[?entry :lang ?lang]
                              [?entry :book ?book]
                              [?entry :source ?source]]
                     :args [{ '?lang lang}]})]
    results))

(defn fetch-all-books [conn]
  (let [
        results   (crux/q
                    (crux/db conn)
                    {:find '[?source ?book ?lang]
                     :where '[[?entry :lang ?lang]
                              [?entry :book ?book]
                              [?entry :source ?source]]})]
    results))

(defn fetch-all-book-names [conn]
  (let [
        results   (crux/q
                    (crux/db conn)
                    {:find '[ ?book]
                     :where '[[?entry :book ?book]
                              [?entry :source ?source]]})]
    results))

(defn put-headings-in-web [book]
  (let [chapters (fetch-chapter-numbers conn "Himlit" book "english")]
    (for [ch chapters]
      (let [headings (fetch-chapter-verse-headings conn "Himlit" book "english" ch)
            x (tap> headings)
            entries (map #(assoc % :crux.db/id (keyword "WEB" (str book "_english_" (System/currentTimeMillis)))
                                   :source "WEB") headings)
            transactions (map #(conj [] :crux.tx/put %) entries)]
        (tap> (vec transactions))))))



