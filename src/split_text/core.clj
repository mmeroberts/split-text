(ns split-text.core
  (:require [split-text.config :refer :all]
            [split-text.db :refer :all]
            [split-text.io :refer :all]
            ;[hickory.core :as h]
            [com.rpl.specter :refer :all]
            [hickory.select :as s]
            [clojure.string :as str]
            [clojure.data.json :as json]))



 
(defn find-first-string [c]
  (some #(when (string? %) %) c))



(defn set-class [sp class]
  (assoc sp :class class))

(defn set-sub-type [sp class]
  (assoc sp :sub-type class))

(defn classify-content [sp content]
  (if (empty? content)
    (set-class sp :empty)
    (let [blank (str/blank? (str/replace content "\u00a0" ""))
          bo (some?(some #(if (>= (int %) 3840) %) content))
          back (str/includes? content "/")
          chapter (not (nil? (re-find #"\([0-9]*\)" content)))
          numb (not (nil? (re-find #"^[0-9][\-0-9]*" content)))
          pass-one (cond blank (set-class sp :blank)
                         bo (set-class sp :bo)
                         back (set-class sp :back)
                         (not bo) (set-class sp :eng)
                         :else (set-class sp :unknown))
          pass-two (if numb (set-sub-type pass-one :verse-number)
                            pass-one)
          pass-three (if chapter (set-sub-type pass-two :chapter-number)
                              pass-two)]
      pass-three)))


(defn classify-element [element]
  (cond (map? element) (let [content (:content element)]
                         (if (and (= (count content) 1) (string? (get content 0)))
                           (classify-content element (get content 0))
                           element))
        (string? element) (let [new-map (assoc {} :content element)]
                            (classify-content new-map element))))


;(def hickory-doc (as-hickory-read-file filename))

(defn get-body-tags [doc]
  (s/select (s/child (s/tag :body) (s/tag :div))  doc))

(defn get-subcontent [content]
  (select [ALL :content ALL map? map? (submap [:tag :content])] content))
; just tag and content
(defn get-stripped-content [content]
  (transform [ALL  :content ALL map? (submap [:attrs :type])] NONE content))


(defn append-index [coll]
  (vec(map-indexed #(assoc %2 :index %1) coll)))
;
(defn append-uuid [coll]
  (vec(map #(assoc %1 :_id (get-uuid-str)) coll)))

(defn append-part-uuid [ coll part-name]
  (vec (map-indexed #(assoc %2
                       :_id (str part-name ":" (get-uuid-str))
                       :index (* (int %1) 100)) coll)))
;
;
(defn handle-sub-maps [content]
  "Expected input is a vector or two vectors.  If second vector last item is a map extract content and rebuild without map"
  (for [s content]
    (let [ff (first s)
          f (first (last s))
          l (last (last s))]
      (if (map? l)
        (let [c (:content l)]
          (if (and (= (count c) 1) (string? (get c 0)))
            s
            (let [new (assoc l :content (into [] (for [m c] (if (map? m) (get (:content m)0) m))))]
              (conj [] ff (conj [] f new)))))
        s))))

(defn set-chapter-number [doc]
  "takes in compact form of a vector of [[outtag details] [inner tage details]] and looks for chapter tags"
  (loop [i 0 ch 0 output '[]]
    (if (< i (count doc))
      (let [sp (get doc i)
            f (first sp)
            ff (first f)
            l (last sp)
            fl (first l)
            m (last l)
            chapter? (contains? (set (vals m)) :chapter-number)
            r (if chapter? (assoc ff :chapter (inc ch))
                           (assoc ff :chapter ch))]
        (recur (inc i) (if chapter? (inc ch) ch) (conj output (conj '[] (conj '[] r) l))))
      output)))

(defn set-verse-number [doc]
  "takes in compact form of a vector of [[outtag details] [inner tage details]] and looks for verse tags"
  (loop [i 0 vn "0" output '[]]
    (if (> i (count doc))
      output
      (let [sp (get doc i)
            f (first sp)
            ff (first f)
            l (last sp)
            fl (first l)
            m (last l)
            z (get (:content m) 0)
            verse-number? (contains? (set (vals m)) :verse-number)
            r (cond verse-number? (assoc ff :verse (re-find #"^(?: )?[-0-9]+" z))
                    (= (:tag ff) :p) (assoc ff :verse vn)
                    :else ff)]
        (recur (inc i) (if verse-number? (re-find #"^(?: )?[0-9]+" z) vn) (conj output (conj '[] (conj '[] r) l)))))))




;(def db-stripped-content (append-uuid book-name stripped-content))



(defn compact-format [content]
  (select [ALL (collect (submap [:_id :index :tag]))  :content INDEXED-VALS ] content))

(defn apply-classfication [content]
  (vec(transform [ ALL LAST LAST] classify-element content)))



(defn process-doc [filename]
  (-> (as-hickory-read-file filename)
      (get-body-tags)
      (get-subcontent)
      (get-stripped-content)
      (append-index)
      (compact-format)
      (handle-sub-maps)
      (apply-classfication)
      (set-chapter-number)
      (set-verse-number)))
      ;(append-uuid book-name))
      ;)

