(ns split-text.core
  (:require [split-text.config :refer :all]
            [split-text.db :refer :all]
            [split-text.io :refer :all]
            [split-text.crux :refer :all]
            [split-text.meta :refer :all]
            [split-text.inwards :refer :all]
            [split-text.outwards :refer :all]
            [crux.api :as crux]
            [clojure.string :as str]
            [com.rpl.specter :refer :all]
            [hickory.render :as hr]
            [hiccup2.core :as h]))
            ;[com.rpl.specter :refer :all]
            ;[hickory.select :as s]
            ;[clojure.string :as str]
            ;[clojure.data.json :as json]))

(def doc (process-doc filename))

;(def dbr (get-ready-for-crux doc))

;;(load-content cruxdb dbr)



;(str/join ""(select [ALL LAST] (get-text cruxdb "James" 1 "1" :bo)))

(defn join-content [c]
  (for [e c]
    (let [[f l] e
          o (str/join ""(reduce into [] (map #(:content %) l)))
          x (conj [] f o)] x)))



(->> (output-bo-book-content doc)
     (group-by (juxt :chapter :vint :index :tag :class))

     ;(join-content)
     (sort))


     ;(map #(:content %))
     ;(reduce into []))