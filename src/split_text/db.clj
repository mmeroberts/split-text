(ns split-text.db
  (:require [split-text.config :refer :all]
            [datahike.api :as d]))

(def cfg {:store {:store {:backend :pg :host "localhost" :port 5432 :username "postgres" :password "postgres" :dbname "pg_example"}};{:backend :file :path db}           ;string
          :schema-flexibility :write})
(d/delete-database cfg)

(d/create-database cfg)

(def conn (d/connect cfg))

(d/transact conn [{:db/ident :book
                   :db/valueType :db.type/string
                   :db/cardinality :db.cardinality/one}
                  {:db/ident :lang
                   :db/valueType :db.type/keyword
                   :db/cardinality :db.cardinality/one}
                  {:db/ident :type
                   :db/valueType :db.type/keyword
                   :db/cardinality :db.cardinality/one}
                  {:db/ident :chapter
                   :db/valueType :db.type/string
                   :db/cardinality :db.cardinality/one}
                  {:db/ident :verse-number
                   :db/valueType :db.type/string
                   :db/cardinality :db.cardinality/one}
                  {:db/ident :index
                   :db/valueType :db.type/number
                   :db/cardinality :db.cardinality/one}
                  {:db/ident :lines
                   :db/valueType   :db.type/ref
                   :db/cardinality :db.cardinality/many}
                  {:db/ident :text/line-no
                   :db/valueType :db.type/number
                   :db/cardinality :db.cardinality/one}
                  {:db/ident :text/line
                   :db/valueType :db.type/string
                   :db/cardinality :db.cardinality/one}])

(defn add_entries [conn md book]
  (for [e md]
    (let [entry_id (d/tempid -1)
          initial-entry (select-keys e [:lang :type :index :verse-number :chapter])
          line-ids (vec (for [i (range (count (:fulltext e)))] (d/tempid -1)))
          linesvec (vec (:fulltext e))
          lines (into [] (for [i (range (count (:fulltext e)))]
                  (let [id (nth line-ids i)
                        line (nth linesvec i)
                        lineno (first line)
                        text (last line)]
                    (assoc {} :db/id id :text/line-no lineno :text/line text))))
          entry (conj  {:db/id entry_id :book book :lines line-ids} initial-entry )
          transaction (conj lines entry)]
      (d/transact conn transaction)
       )))

(comment
  (d/transact conn [{
                     :lang         :english,
                     :type         :verse,
                     :index        23,
                     :verse-number "7",
                     :chapter      "1",
                     :fulltext     ["Look! He (Jesus) will come on the clouds! Not only will everyone see him but those who pierced him [with a spear] will also see him. All the peoples of the earth will mourn loudly because of him. His coming will surely happen! Amen. (So be it). "]}])

  (def t23 {:text         "&quot;དེར་ག་རེ་ཡོད་པ་ཚང་མའི་བྱུང་རྐྱེན་དེ་ང་ཡིན་པ་དང་&#xf0d;&emsp;ཚང་མ་མཐའ་ཅན་བྱེད་མཁན་དེ་ང་ཡིན་ཟེར་ནས་ཀུན་དབང་གཙོ་བོ་དཀོན་མཆོག་གིས་གསུང་གི་ཡོད་རེད།&emsp;ང་ལ་དབང་ཆ་ཆ་ཚང་ཡོད&#xf0d;&emsp;ང་གསོན་པོ་ཡིན་པ་དང་།&emsp;ང་ག་དུས་ཡིན་ནས་གསོན་པོ་ཡིན།&emsp;ང་ཡོང་གི་ཡིན།&quot; ",
            :lang         :bo,
            :type         :verse,
            :index        24,
            :verse-number "8",
            :chapter      "1",
            :fulltext     ["&quot;དེར་ག་རེ་ཡོད་པ་ཚང་མའི་བྱུང་རྐྱེན་དེ་ང་ཡིན་པ་དང་&#xf0d;&emsp;ཚང་མ་མཐའ་ཅན་བྱེད་མཁན་དེ་ང་ཡིན་ཟེར་ནས་ཀུན་དབང་གཙོ་བོ་དཀོན་མཆོག་གིས་གསུང་གི་ཡོད་རེད།&emsp;ང་ལ་དབང་ཆ་ཆ་ཚང་ཡོད&#xf0d;&emsp;ང་གསོན་པོ་ཡིན་པ་དང་།&emsp;ང་ག་དུས་ཡིན་ནས་གསོན་པོ་ཡིན།&emsp;ང་ཡོང་གི་ཡིན།&quot; "]})
  (add_entries conn mdcproc "Revelation")

  (d/transact conn [(select-keys t23 [:lang :type :index :verse-number :chapter :fulltext])])
  (sort (d/q '[:find ?e ?i ?b ?c ?v ?f ?i
               :in $  ?l
               :where
               [?e :book ?b]
               [?e :chapter ?c]
               [?e :index ?i]
               [?e :verse-number ?v]
               [?e :fulltext ?f]
               [?e :lang ?l]]
             @conn  :english))
  (sort (d/q '[:find ?i ?e  ?b ?c ?v  ?i ?ls ?t
               :in $  ?c ?v ?l
               :where
               [?e :book ?b]
               [?e :chapter ?c]
               [?e :index ?i]
               [?e :verse-number ?v]
               [?e :lang ?l]
               [?e :lines ?ls]
               [?ls :text/line ?t]]
             @conn "5" "9" :bo))

        (d/q '[:find ?e  ?ls
               :in $  ?c ?v ?l
               :where
               [?e :book ?b]
               [?e :chapter ?c]
               [?e :verse-number ?v]
               [?e :lang ?l]
               [?e :lines ?ls]]
             @conn "5" "9" :bo)

  (sort(d/q '[:find ?ls ?ln ?t
         :where
         [?ls :text/line-no ?ln]
         [?ls :text/line ?t]]
       @conn )))



  ;
  ;










;;;;;;;; DB;;;;;;;;

