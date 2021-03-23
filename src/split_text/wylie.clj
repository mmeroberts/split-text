(ns split-text.wylie
  (:require [clojure.string :as str]
            [com.rpl.specter :refer [transform ALL]])
  (:import org.rigpa.wylie.Wylie))

(defn prepare-for-wylie
  "This will replace emsp entities and also She entities before passing it to the Wylie transliteration"
  [l]
  (-> l
      (str/replace #"&emsp;" " ")
      (str/replace #"&#xf0d;" "།")))

(defn add-emsp
  "replace the spaces between senstences with emp entity"
  [l]
  (str/replace l #" " "&emsp;"))


(defn to-wylie [l]
  (let [wl (Wylie.)
        st (prepare-for-wylie l)
        final (.toWylie wl st)]
    final))

(defn from-wylie [l]
  (let [wl (Wylie.)
        fw (add-emsp(.fromWylie wl l))
        final (str/replace fw #"\*" "\\\\*")]
    final))

(defn post-process-wylie
  "This post processes the wylie ready for display ( this means removing the escaped characters so the display normally"
  [l]
  (-> l
      (str/replace #"\\\[" "@")
      (str/replace #"\\\]" "#")
      (str/replace #"(\[|\])" "")
      (str/replace #"@" "[")
      (str/replace #"#" "]")
      (str/replace #"_" "&ensp;")
      (str/replace #"\*" "&ensp;")
      (str/replace  #"(\{(.+?)\})" split-text.config/name-highlight-style)))


(defn handle-wylie [text]
  (let [wylie (to-wylie text)
        back (from-wylie wylie)
        post (post-process-wylie wylie)]
    (when (not= (compare text back) 0) (println "failed:" text "#" wylie "#" back))
    post))

(defn get-wylie-lines [entry]
  (let [first (transform [ALL :lines ALL :line] handle-wylie entry)]
    (transform [ALL :lang] (fn [x] "wylie") first)))


(comment
  (def s "ལོ་རྒྱུས་འདིས་དཀོན་མཆོག་གིས་{ཡེ་ཤུ་མ་ཤི་ཀ}་ལ་བསྟན་གནང་པའི་(མ་འོངས་པར་འབྱུང་རྒྱུ་ཡིན་པའི་དོན་རྐྱེན་དེ་ཚོ་) འགྲེལ་བཤད་རྒྱབ་ཀྱི་ཡོད་རེད&#xf0d;&emsp;{ཡེ་ཤུ}་གིས་འདི་ཚོ་[ཁོང་གི་རྗེས་འབྲང་ཡིན་པའི་] རང་གི་ཞབས་ཕྱི་ཚོ་ལ་གསལ་སྟོན་གནང་རྒྱུ་དེ་དཀོན་མཆོག་གི་ཐུགས་འདོད་ཡིན་པ་རེད&#xf0d;&emsp;དོན་རྐྱེན་འདི་ཚོ་མགྱོགས་པོ་འབྱུང་དགོས་ཀྱི་ཡོད་རེད&#xf0d;&emsp;{ཡེ་ཤུ}་གིས་དོན་རྐྱེན་འབྱུང་གྲབས་ཡོད་པ་དེ་ཚོ་ཁོང་གི་ཞབས་ཕྱི་{ལྗཱོན}་ལ་གསལ་སྟོན་གནང་རྒྱུའི་ཆེད་དུ་ཁོང་གི་ཕོ་ཉ་དེ་བཏང་གནང་པ་རེད&#xf0d;&emsp;")

  (def s' (prepare-for-wylie s))
  (def x (to-wylie s))
  (def y (from-wylie x))
  (= split-text.wylie/s   split-text.wylie/y)
  (prn y)

  (def x' (to-wylie s'))
  (def y' (from-wylie x'))

  (= split-text.wylie/s'   split-text.wylie/y')
  (def z' (add-emsp y'))
  (= split-text.wylie/s   split-text.wylie/z')

  (to-wylie "ཁོས་ལུག་དགུ་བཅུ་གོ་དགུ་དེ་ཚོ་རི་ཟུར་ལ་རྩྭ་ཟ་འཇུག་ནས།")
  (to-wylie s)
  (handle-wylie s)
  (post-process-wylie "dj[\\[]udj[\\]]s[(]k[({]s[{]kdddd[}]dkdkdk")
  ,)
