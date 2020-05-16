(ns split-text.core
  (:require [split-text.config :refer :all]
            [split-text.db :refer :all]
            [split-text.io :refer :all]
            ;[hickory.core :as h]
            [com.rpl.specter :refer :all]
            [hickory.select :as s]
            [clojure.string :as str]))



 
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

