(ns split-text.config
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def config-atom (atom {}))

(defn handle-reference [ch ref]
  (cond (= ref "ALL") ch
        (re-find #"^\d+$" ref) [(Integer/parseInt ref)]
        (re-find #"^\d+-\d+$" ref) (let [x (re-matches #"^(\d+)-(\d+)$" "1-3")
                                         n (Integer/parseInt (second x))
                                         m (inc(Integer/parseInt (last x)))]
                                     (range n m))))



(defn get-config [options]
  (let [{:keys [config]} options
        config-file (when (some? config) (edn/read-string(slurp config)))
        directory (if (nil? (:directory config-file))  {:directory (str/replace config #"\\config\\config.edn" "")} nil)
        results (merge config-file directory options)]
    (tap> results)
    (swap! config-atom conj results)
    results))

(comment
  (str/replace "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation\\config\\config.edn" #"\\config\\config.edn" "")
  (get-config {:config "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation\\config\\config.edn" :source "Himlit"})
  ,)
(def host "192.168.1.109")
(def port "5984")
(def user "admin")
(def passwd "admin")
(def db "texts")



(def server "http://192.168.1.109:5984")


;;; Config for transformation
(def stylesheet ".\\resources\\css\\main.css")
(def name-highlight-style  "<span class=\"nameHighlight\">$2</span>")
(def name-highlight  "{$2}")
(def name-highlight-h2  "$2")
(def extra-format  "$2")
;(def name-highlight  "{$2}")
(def ignore-quotations? false)
(def print_debug? true)
(defn debug [& more]
  (when print_debug?
    (println (str more))))

;(debug "hello")

(def row-div-open "<div class=\"row\">")
(def div-close "</div>")
(def column-div-open "<div class=\"column\">")


(def split-verse-normal #"((?:[-0-9]+)(\D+|[0-9]+,000|[0-9]+,[0-9]+|[0-9]{3}|[0-9]\:[\-0-9]+)*)")
(def split-verse-24 #"((?:[-0-9]+)(\D+|[0-9]+,000|[0-9]+,[0-9]+|[0-9]{3}|[0-9]\:[\-0-9]+|24)*)")

(def bo-brackets "<span class=\"bo-brackets\">$1</span>")

(def original "pre-processing")
(def intermediate "_intermediate")
(def pre-published "pre-published")



(def row-tiny-image "<img width=\"22px\" height=\"29px\"  src=\"data:image/jpg;base64,/9j/4AAQSkZJRgABAQEAYABgAAD/2wBDAAMCAgMCAgMDAwMEAwMEBQgFBQQEBQoHBwYIDAoMDAsK\nCwsNDhIQDQ4RDgsLEBYQERMUFRUVDA8XGBYUGBIUFRT/2wBDAQMEBAUEBQkFBQkUDQsNFBQUFBQU\nFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBT/wAARCAA6ACwDASIA\nAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQA\nAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3\nODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWm\np6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QAHwEA\nAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREAAgECBAQDBAcFBAQAAQJ3AAECAxEEBSEx\nBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElK\nU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3\nuLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwD2z9tr\n9t7WvAfitPhR8KYpLjxzK0UV9qUNuJ5LSSYKYbW2iIIkuH3oSSpVQ6qAzsTF5lov/BO346/Eyxi1\nbx/8VG0nU5R5gtby+utXuIsgZWR/MVEYYxtjeROBhqZ/wTK8P2vxS/aB+KfxO1uKO41uyIntxIit\n5c2oz3LzSqcfKwWF4wR/DNIvQ4r9MmZYYy2MKvPyj+lfTYmt/ZLWGoRXOknKTV3dq9lfpZmEf3l2\n9j84F/4JWeO02gfGaEAHI/4l1xx7gfaKev8AwSu8cxxFV+M8PfA/sy4A/wDSn2r6n+Hf7cHwY+KU\nPiSTQfGCMPD9nNqV+t5aTWzrZxAtJcKroC6qASQBuHGVGRnmIf2gvj14tmhPhf8AZwutO0q9INrq\n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\"></div>")

(def row-normal-image "<div class=\"logo\"> <img  src=\"data:image/jpg;base64,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\nY92q0AwAAAAASUVORK5CYII=\"></div>")

(defn get-row-tiny-image [id]
  (str "<div class=\"logo\" id=\"" id "\">" row-tiny-image div-close))

(def dbcfg {:store {:backend :pg :host "localhost" :port 5432 :username "postgres" :password "postgres" :dbname "pg_example"} :name "himlit"  :schema-flexibility :write})


