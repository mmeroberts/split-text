(ns split-text.inwards
  (:require [split-text.config :refer :all]
            [split-text.db :refer :all]
            [split-text.io :refer :all]
            [split-text.crux :refer :all]
            [split-text.meta :refer :all]
            [crux.api :as crux]
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
  ;(println "cc:" content)
  (if (empty? content)
    (set-class sp :blank)
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
                         (classify-content element (str/join "" content)))
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

(defn append-crux-index [coll nsp]
  (map-indexed #(assoc %2  :book nsp :crux.db/id (keyword nsp (str  nsp "-" %1))) coll))

(defn append-uuid [coll]
  (vec(map #(assoc %1 :_id (get-uuid-str)) coll)))

(defn append-part-uuid [ coll part-name]
  (vec (map-indexed #(assoc %2 :_id (str part-name ":" (get-uuid-str))
                               :record_index %1) coll)))

(defn append-part-uuid [ coll part-name]
  (vec (map-indexed #(assoc %2 :_id (str part-name ":" (get-uuid-str))
                               :record_index %1) coll)))
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
  (loop [i 0 ch 0 last-tag :p output '[]]
    (if (< i (count doc))
      (let [sp (get doc i)
            f (first sp)
            ff (first f)
            l (last sp)
            fl (first l)
            m (last l)
            this-ch (if (and (= last-tag :p) (= (:tag ff) :h1)) (inc ch) ch)
            next-last (:tag ff)
            r (assoc ff :chapter this-ch)]
        (recur (inc i) this-ch next-last (conj output (conj '[] (conj '[] r) l))))
      output)))

(defn set-verse-number [doc]
  "takes in compact form of a vector of [[outtag details] [inner tage details]] and looks for verse tags"
  (loop [i 0 vn "0" output '[]]
    (if (>= i (count doc))
      output
      (let [sp (get doc i) ;[[{:index 0, :tag :p, :chapter 0}] [0 {:content "A2A Part 11: Epistles only", :class :eng}]]
            f (first sp)   ;[{:index 0, :tag :p, :chapter 0}]
            ff (first f)   ;{:index 0, :tag :p, :chapter 0}
            l (last sp)    ;[0 {:content "A2A Part 11: Epistles only", :class :eng}]
            fl (first l)   ;0
            m (last l)     ;{:content "A2A Part 11: Epistles only", :class :eng}
            z (get (:content m) 0) ;"A2A Part 11: Epistles only"
            verse-number? (or (and (= (:tag f) :p) (= (:chapter ff) 0))
                              (= (:tag f) :h1)
                              (contains? (set (vals m)) :verse-number))
            r (if verse-number? (assoc ff :verse (re-find #"^(?: )?[-0-9]+" z))
                    ;(= (:tag ff) :p) (assoc ff :verse vn)
                                (assoc ff :verse vn))]
        (recur (inc i) (if verse-number? (re-find #"^(?: )?[-0-9]+" z) vn) (conj output (conj '[] (conj '[] r) l)))))))


(defn compact-format [content]
  (select [ALL (collect (submap [:_id :index :tag]))  :content INDEXED-VALS ] content))

(defn transform-second-item [item]
  ;(println item)
  (cond (string? item) (assoc {} :content (conj '[] item))
        (vector? (:content item)) (assoc item :content (conj '[] (str/join (:content item))))))




(defn apply-classfication [content]
  (let [intermediate (transform [ALL LAST LAST] transform-second-item content)]
    (vec (transform [ALL LAST LAST] classify-element intermediate))))

(defn remove-empty-content [content]
  (remove nil? (for [sp content]
                 (let [c (select [LAST LAST :content FIRST] sp)
                       cl (select [LAST LAST :class ] sp)]
                   (if (or (not= (get cl 0) :blank) (not (empty? c)))
                     sp)))))

(defn set-db-format [content]
  (vec
    (for [sp content]
      (let [f (first (first sp)) ; get the map in first entry
            si (first (last sp)) ; index in second vector
            l (last(last sp)) ; map in second vector
            lt (:tag l) ; tag in second vector - to be renamed sub-tag
            c (if (= lt :u) [ (str/join "" ["$" (get (:content l) 0) "$"])] (:content l)) ; actual content of sp
            cl (:class l)
            sc (:sub-type l)]
        (if (not (nil? sc))
          (assoc f :subindex si :subtag lt :content c :class cl :sub-type sc)
          (assoc f :subindex si :subtag lt :content c :class cl))))))


(defn link-verses [doc]
  "loop through the entries looking for verse-numbers
    read ahead until non-blank check the class
    change class of verse to match first non-blank
      Note some verse numbers are in the text so need to check if verse-number is isolated"
  (loop [i 0 output '[]]
    (if (> i (count doc))
      output
      (let [this (get doc i)
            verse-number? (contains? (set (vals this))  :verse-number)
            j (inc i)
            this-content (if (vector? (:content this)) (get (:content this) 0) (:content this))
            check (if (not (nil? this-content)) (re-find #"^(?: *)[-0-9]+ .+" this-content) this-content)
            ret-map (if verse-number?
                      (if (empty? check)
                        (let [xt (loop [n j]
                                   (if (or (>= n (count doc)) (not (contains? (set (vals (get doc n))) :blank)))
                                     n
                                     (recur (inc n))))
                              next (get doc xt)
                              next-class (:class next)
                              new-this (assoc this :class next-class)]
                          new-this)
                        this)
                      this)]
        (recur (inc i) (conj output ret-map))))))

(defn set-verse-int [content]
  (for [sp content]
    (let [vrs (:verse sp)]
        (if (not (nil? vrs))
          (let [vint (Integer. (re-find #"^[0-9]+" vrs))]
            (assoc sp :vint vint))
          sp))))

(defn set-title [content]
  (for [sp content]
    (let [ch (:chapter sp)
          vrs (:verse sp)]
      (if (and (= ch 0) (= vrs "0"))
        (assoc sp :tag :cite)
        sp))))





(defn process-doc [filename]
  (-> (as-hickory-read-file filename)
      (get-body-tags)
      (get-subcontent)
      (get-stripped-content)
      (append-index)))
      ;(compact-format)
      ;(handle-sub-maps)
      ;(apply-classfication)
      ;(set-chapter-number)
      ;(set-verse-number)
      ;(remove-empty-content)
      ;(set-db-format)
      ;(link-verses)
      ;(set-title)
      ;(set-verse-int)
      ;(vec)))



(defn get-ready-for-db [content]
  (-> content
      (append-part-uuid book-name)))

(defn get-ready-for-crux [content]
  (-> content
      (append-crux-index book-name)))




;(str/join ""(select [ALL LAST] (get-text cruxdb "James" 1 "1" :bo)))

;(str/join ""(reduce into [] (map #(:content %) x)))

