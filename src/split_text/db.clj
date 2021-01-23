(ns split-text.db
  (:require    [clojure.string :as str]
               [clojure.java.io :as io]
               [crux.api :as crux]))




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



(defn make-entry [source book  l]
  (let [index (:index l)
        id (keyword "web" (str book "_" index))
        chapter (:chapter l)
        verse-number (:verse-number l)
        linesvec (vec (:fulltext l))
        lines (into [] (for [i (range (count (:fulltext l)))]
                         (let [line (nth linesvec i)
                               lineno (first line)
                               text (last line)]
                           (assoc {} :text/line-no lineno :text/line text))))]
    (assoc {} :crux.db/id id
           :index index :source source :lang :english
           :book book :chapter chapter :verse-number verse-number
           :lines  lines)))

(defn add_entries [conn source book md]
  (let [firstpass (map #(make-entry source book %) md)
        secondpass (map #(conj [] :crux.tx/put %) firstpass)]
     (crux/submit-tx conn (vec secondpass))))



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
  ,)



  ;
  ;










;;;;;;;; DB;;;;;;;;

