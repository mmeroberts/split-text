(ns split-text.crux
  (:require
    [split-text.config :refer :all]
    [crux.api :as crux]
    [clojure.java.io :as io]))

(defn start-rocks-node [storage-dir]
  (crux/start-node {:crux.node/topology '[crux.standalone/topology
                                          crux.kv.rocksdb/kv-store]
                    :crux.kv/db-dir (str (io/file storage-dir "db"))}))

(defonce cruxdb (start-rocks-node db))

(defn load-content [db content]
  (for [sp content]
    (crux/submit-tx
      db
      [[:crux.tx/put
        sp]])))


(defn get-text-by-chapter-and-verse [db book chapter verse language]
  (crux/q (crux/db db)
          {:find '[e b i s ch vs t st c]
           :where '[[e :book b]
                    [e :chapter ch]
                    [e :verse vs]
                    [e :class l]
                    [e :tag t]
                    [e :subtag st]
                    [e :content c]
                    [e :index i]
                    [e :subindex s]]
           :args [{'b book 'ch chapter 'vs verse 'l language}]
           :order-by '[[i :asc] [s :asc]]}))

(defn query-chapter-numbers [db book]
  (crux/q (crux/db db)
          {:find '[ch]
           :where '[[e :book b]
                    [e :chapter ch]]
           :args [{'b book}]}))

(defn query-verse-numbers [db book]
  (crux/q (crux/db db)
          {:find '[ch vs]
           :where '[[e :book b]
                    [e :chapter ch]
                    [e :verse vs]]
           :args [{'b book}]}))

(defn query-verse-numbers-by-language [db book lang]
  (crux/q (crux/db db)
          {:find '[ch vs]
           :where '[[e :book b]
                    [e :chapter ch]
                    [e :verse vs]
                    [e :class l]]
           :args [{'b book 'l lang}]}))

(defn query-verse-numbers-by-chapter [db book chapter]
  (crux/q (crux/db db)
          {:find '[ch vs]
           :where '[[e :book b]
                    [e :chapter ch]
                    [e :verse vs]]
           :args [{'b book 'ch chapter}]
           :order-by [['vs]]}))

(defn query-book-metadata [db book chapter]
  (crux/q (crux/db db)
          {:find '[ch minv maxv]
           :where '[[e :book b]
                    [e :chapter ch]
                    [e :type "metadata"]
                    [e :min-verse-number minv]
                    [e :max-verse-number maxv]]
           :args [{'b book 'ch chapter}]}))

;(crux/entity (crux/db cruxdb) :James/James-14)