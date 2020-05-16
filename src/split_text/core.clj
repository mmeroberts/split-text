(ns split-text.core
  (:require [split-text.config :refer :all]
            [split-text.db :refer :all]
            [split-text.io :refer :all]
            ;[hickory.core :as h]
            [com.rpl.specter :refer :all]
            [hickory.select :as s]
            [clojure.string :as str]))







;(defn as-hiccup-read-file [filename]
;  (let [raw (slurp filename)
;        parsed-doc (h/parse raw)]
;    (h/as-hiccup parsed-doc)))

;(defn get-hickory-tags [text tag]
;  (s/select (s/child (s/tag tag)) text))

;(defn get-hiccup-tags [text tag]
;  (s/select (s/child (s/tag tag)) text))
;
;(defn get-name-set [text]
;  (set (map #(if (string? (get (:content %) 0)) (get (:content %) 0) (get (:content (get (:content %) 0)) 0)) (s/select (s/child (s/tag :u)) text))))

;(defn get-spans [text]
;  (s/select (s/child (s/tag :span)) text))


;(defn get-bo-spans [text]
;  (s/select (s/child (s/attr :style #(.startsWith % "font-size:20.0pt;font-family:\n\"Microsoft Himalaya\""))) text))
;
;(defn output-spans [spans output-filename]
;  (spit output-filename (with-out-str (pprint/pprint spans))))

;(defn append-crux-index [coll nsp]
;  (map-indexed #(assoc %2 :index %1 :crux.db/id (keyword nsp (str  "j-" %1))) coll))
;
;	(defn start-rocks-node [storage-dir]
;   (crux/start-node {:crux.node/topology '[crux.standalone/topology
;                                           crux.kv.rocksdb/kv-store]
;                     :crux.kv/db-dir (str (io/file storage-dir "db"))}))

;(defn clean-str [st]
;      (when (not-empty st)
;        (str/replace (str/replace (str/replace st "\ufffd" "" ) "\n" " ") "\u00a0" " ")))
;
;(defn clean-span [s]
;  (if (contains? #{:h1 :h2 :p :span} (:tag s))
;    (let [content (:content s)
;          clean-content (for [c content]
;                          (cond (string? c) (clean-str c)
;                                (map? c) (let [innerc (clean-str (get content 0))]
;                                           (assoc c :content (vector innerc)))))]
;       (assoc s :content (vec clean-content)))
;
;    s))

 
(defn find-first-string [c]
  (some #(when (string? %) %) c))



(defn set-class [sp class]
  (assoc sp :class class))

(defn set-sub-type [sp class]
  (assoc sp :sub-type class))

(defn classify-content [sp content]
  (if (nil? content)
    (set-class sp :empty)
    (let [blank (str/blank? (str/replace content "\u00a0" ""))
          bo (and (not (empty? content)) (some?(some #(if (>= (int %) 3840) %) content)))
          ;back (str/includes? content "/")
          numb (not (nil? (re-find #"^[0-9][\-0-9]*$" content)))]
      (cond blank (set-class sp :blank)
            numb (set-sub-type sp :verse-number)
            bo (set-class sp :bo)
            ;back (set-class sp :back)
            (not bo) (set-class sp :eng)
            :else (set-class sp :unknown)))))


(defn classify-span [sp]
  (if (contains? #{:h1 :h2 :p :span} (:tag sp))
    (let [content (:content sp)
          initial_str (find-first-string content)]
         (classify-content sp initial_str))
    (when (= (:tag sp) :u)
      (let [new-content (select [:content ALL :content ALL] sp)
            new-span (assoc sp :content new-content)]
        (set-class new-span :name)))))


(defn process-span [sp]
  (-> sp
      ;(clean-span)
      (classify-span)))


(defn classifiy-top-level-element [element]
  (let [content (:content element)]
    (if (and (= (count content) 1) (string? (get content 0)))
      (classify-content element (get content 0))
      element)))


;(defn cool-filter [[k v] l]
;  (filter #(= (k %) v) l))


;(def spans (get-spans hickory-doc))

;(def idx-spans (vec (append-index spans "James")))

;(def processed-spans (map #(process-span %) (vec idx-spans)))

;(def bo-spans (cool-filter [:class :bo] processed-spans))
;


  






(def hickory-doc (as-hickory-read-file filename))

(def contents (s/select (s/child (s/tag :body) (s/tag :div))  hickory-doc))
(def subcontent (select [ALL :content ALL map? map? (submap [:tag :content])] contents))
; just tag and content
(def stripped-content (transform [ALL  :content ALL map? (submap [:attrs :type])] NONE subcontent))


(defn get-uuid-str []
  (let [{:keys [status headers body error] :as resp} (client/get (str server "/_uuids"))]
    (if error
      nil
      (first (get (json/read-str body) "uuids")))))
;
(defn append-uuid
  ([coll]  (vec(map-indexed #(assoc %2 :_id (get-uuid-str) :index %1) coll)))
  ([coll part-name] (vec (map-indexed #(assoc %2 :_id (str part-name ":" (get-uuid-str)) :index %1) coll))))
;
;


(def db-stripped-content (append-uuid stripped-content book-name))

(def classified-db-stripped-content  (transform [ALL] classifiy-top-level-element  (transform [ALL :content ALL map?] process-span db-stripped-content)))


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