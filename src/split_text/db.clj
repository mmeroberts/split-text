(ns split-text.db
  (:require [split-text.config :refer :all]
            [clojure.data.json :as json]
            [clj-http.client :as client]
            [lambdaisland.uri :refer [uri join]]))

;;;;;;;; DB;;;;;;;;




(defn db-uri [ host port user passwd db]
  (str (assoc (uri (str "/" db))
         :scheme "http"
         :user user
         :password passwd
         :host host
         :port port
         :path (str "/" db))))

(def texts-db (db-uri host port user passwd db))


(defn load-full-document [uri document]
  (map #(client/post uri {:content-type :json :body (json/write-str %)}) document))
