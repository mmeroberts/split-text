(ns split-text.crux
  (:require
    [split-text.config :refer :all]
    [crux.api :as crux]
    [clojure.java.io :as io]))

(defn start-rocks-node [storage-dir]
  (crux/start-node {:crux.node/topology '[crux.standalone/topology
                                          crux.kv.rocksdb/kv-store]
                    :crux.kv/db-dir (str (io/file storage-dir "db"))}))

;(def cruxdb (start-rocks-node db))

(defn load-content [db content]
  (for [sp content]
    (crux/submit-tx
      db
      [[:crux.tx/put
        sp]])))


(defn get-text [db book chapter verse language]
  (crux/q (crux/db db)
          {:find '[e b i s ch vs c]
           :where '[[e :book b]
                    [e :chapter ch]
                    [e :verse vs]
                    [e :class l]
                    [e :content c]
                    [e :index i]
                    [e :subindex s]]
           :args [{'b book 'ch chapter 'vs verse 'l language}]
           :order-by '[[i :asc] [s :asc]]}))