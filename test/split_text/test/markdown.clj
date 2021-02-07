(ns split-text.test.markdown
  (:require [clojure.test :refer :all]
            [split-text.markdown :refer :all]
            [split-text.markdownout :refer :all]))

(def test-text-in [{:text " 7", :lang :english, :type :h2, :index 1, :chapter "7"}
                   {:text "The city was laid out like a square. All four sides were the same length. When the angel measured it (he found it was cube-like in shape). Its length was 1,500 miles. It was as wide and as high as it was long. ",
                    :lang :english,
                    :type :verse,
                    :index 3,
                    :chapter "7",
                    :verse-number "16"}
                   {:text "Then [the four living beings and the twenty-four overseers] sang a new song (that had never been sung before). These are the words they sang:",
                    :lang :english,
                    :type :verse,
                    :index 9,
                    :chapter "7",
                    :verse-number "9"}
                   {:text " \"You are worthy to take the scroll and break open its seals.",
                    :lang :english,
                    :type :h4,
                    :index 10,
                    :chapter "7"}
                   {:text " Because you were slaughtered, your life-blood has ransomed ",
                    :lang :english,
                    :type :h4,
                    :index 11,
                    :chapter "7"}
                   {:text " people from all over the world. Now these people (who are ",
                    :lang :english,
                    :type :h4,
                    :index 12,
                    :chapter "7"}
                   {:text " from every race and country) belong to God.", :lang :english, :type :h4, :index 13, :chapter "7"}
                   {:text "Then the angel measured the height [or thickness] of the wall and found it to be 216 feet (according to the human standard used by people). ",
                    :lang :english,
                    :type :verse,
                    :index 15,
                    :chapter "7",
                    :verse-number "17"}
                   {:text "The wall was made of jasper, and the city (and everything in it) was made of pure gold. The gold was as clear as glass. ",
                    :lang :english,
                    :type :verse,
                    :index 17,
                    :chapter "7",
                    :verse-number "18"}
                   {:text "Then I heard how many people had received God's mark. The total number sealed was one hundred and forty four thousand (144,000). They were from the all the tribes of Israel. ",
                    :lang :english,
                    :type :verse,
                    :index 19,
                    :chapter "7",
                    :verse-number "4"}
                   {:text "From each tribe there were 12,000 people.",
                    :lang :english,
                    :type :verse,
                    :index 24,
                    :chapter "7",
                    :verse-number "5"}
                   {:text " 12,000 were from Judah's tribe;", :lang :english, :type :h4, :index 25, :chapter "7"}
                   {:text " 12,000 from the tribe of Reuben;", :lang :english, :type :h4, :index 26, :chapter "7"}
                   {:text " 12,000 from the tribe of Gad.", :lang :english, :type :h4, :index 27, :chapter "7"}
                   {:text " 12,000 were from the tribe of Asher; ", :lang :english, :type :h4, :index 31, :chapter "7", :verse-number "6"}
                   {:text " 12,000 were from the tribe of Naphtali; ", :lang :english, :type :h4, :index 32, :chapter "7"}
                   {:text " 12,000 were from the tribe of Manasseh.", :lang :english, :type :h4, :index 33, :chapter "7"}
                   {:text " 12,000 were from the tribe of Simeon; ",
                    :lang :english,
                    :type :h4,
                    :index 38,
                    :chapter "7",
                    :verse-number "7"}
                   {:text " 12,000 were from the tribe of Levi; ", :lang :english, :type :h4, :index 39, :chapter "7"}
                   {:text " 12,000 were from the tribe of Issachar. ", :lang :english, :type :h4, :index 40, :chapter "7"}
                   {:text "12,000 were from the tribe of Zebulun; ",
                    :lang :english,
                    :type :h4,
                    :index 44,
                    :chapter "7",
                    :verse-number "8"}
                   {:text " 12,000 were from the tribe of Joseph; ", :lang :english, :type :h4, :index 45, :chapter "7"}
                   {:text " 12,000 were from the tribe of Benjamin ", :lang :english, :type :h4, :index 46, :chapter "7"}
                   {:text " John sees a huge crowd dressed in white ",
                    :lang :english,
                    :type :h3,
                    :index 48,
                    :chapter "7",
                    :next-verse "9"}
                   {:text "After this I looked (again) and I saw a huge crowd. There were so many people it was impossible to count them all. There were people from every nation, tribe, people and language (group). They stood facing the throne and the Lamb. They were wearing white clothes and each person carried palm branches in their hands.",
                    :lang :english,
                    :type :verse,
                    :index 50,
                    :chapter "7",
                    :verse-number "9"}])

(def expect-index-1 {:text " 7", :lang :english, :type :h2, :index 1,  :chapter "7", :fulltext {1 " 7"}})
(def expect-index-2
  {:text "Then [the four living beings and the twenty-four overseers] sang a new song (that had never been sung before). These are the words they sang:",
   :lang :english,
   :type :verse,
   :index 9,
   :chapter "7",
   :verse-number "9",
   :fulltext {9 "Then [the four living beings and the twenty-four overseers] sang a new song (that had never been sung before). These are the words they sang:",
              10 "> \"You are worthy to take the scroll and break open its seals.",
              11 "> Because you were slaughtered, your life-blood has ransomed",
              12 "> people from all over the world. Now these people (who are",
              13 "> from every race and country) belong to God."}})
(def expect-index-3  {:text "From each tribe there were 12,000 people.",
                      :lang :english,
                      :type :verse,
                      :index 24,
                      :chapter "7",
                      :verse-number "5",
                      :fulltext {27 "> 12,000 from the tribe of Gad.",
                                 24 "From each tribe there were 12,000 people.",
                                 25 "> 12,000 were from Judah's tribe;",
                                 26 "> 12,000 from the tribe of Reuben;"}})
(def expect-index-4  {:text "12,000 were from the tribe of Asher; ",
                      :lang :english,
                      :type :h4,
                      :index 31,
                      :chapter "7",
                      :verse-number "6",
                      :fulltext {31 "> 12,000 were from the tribe of Asher;",
                                 32 "> 12,000 were from the tribe of Naphtali; ",
                                 33 "> 12,000 were from the tribe of Manasseh."}})

(def md (merge-quotations test-text-in))

(deftest test-merge-quotations
  (let [md (merge-quotations test-text-in)]
    (is (= (first md) expect-index-1))
    (is (= (nth md 2) expect-index-2))
    (is (= (nth md 6) expect-index-3))
    (is (= (nth md 7) expect-index-4))))

(def add-verse-l1 {:index 210,
                   :book "Revelation",
                   :lines [{:line-no 210,
                            :line "དེ་ནས་[སྲོག་ཆགས་བཞི་པོ་དེ་ཚོ་དང་དོ་དམ་པ་ཉི་ཤུ་རྩ་བཞི་དེ་ཚོས་] (དེ་སྔ་རྩ་བ་ནས་བླངས་མ་མྱོང་བའི་) གླུ་གཞས་གསར་པ་ཞིག་བླངས་སོང་&#xf0d;&emsp;ཁོང་ཚོས་བླངས་པའི་གླུའི་ཚིག་དེ་འདི་ཚོ་རེད་འདུག:&ensp;"}
                           {:line-no 211, :line "> །ཁྱེད་རང་ཤོག་དྲིལ་དེ་བླང་ནས་དེའི་ཐེལ་ཙེ་དེ་ཚོ་གཅག་ནས་ཁ་འབྱེད་འོས་པ་འདུག"}
                           {:line-no 212, :line "> །གང་ཡིན་ཟེར་ན་ཁྱེད་རང་བསད་པ་དང་།&emsp;ཁྱེད་རང་གི་ཟུང་ཁྲག་"}
                           {:line-no 213, :line "> དེས་འཛམ་བུ་གླིང་ཡོངས་རྫོགས་ཀྱི་མི་ཚོ་བླུ་ཉོ་བྱས་པ་རེད།"}
                           {:line-no 214, :line "> ད་ལྟ་(མི་རིགས་དང་ལུང་པ་མི་གཅིག་པའི་མི་ཚང་མ་) དཀོན་མཆོག་ལ་བདག་གི་ཡོད་རེད།"}],
                   :type :verse,
                   :source "Himlit",
                   :verse-nos [9],
                   :lang "bo",
                   :chapter "5",
                   :chapter-no 5,
                   :verse-number "9"})

(def add-verse-l2 {:index 291,
                   :book "Revelation",
                   :lines [{:line-no 291, :line "> 12,000 were from the tribe of Asher;"}
                           {:line-no 292, :line "> 12,000 were from the tribe of Naphtali; "}
                           {:line-no 293, :line "> 12,000 were from the tribe of Manasseh."}],
                   :type :h4,
                   :source "Himlit",
                   :verse-nos [6],
                   :lang "english",
                   :chapter "7",
                   :chapter-no 7,
                   :verse-number "6"})

(def expect-add-verse-l1 {:index 210,
                          :book "Revelation",
                          :lines [{:line-no 210,
                                   :line "<span class=\"vn\">9</span>དེ་ནས་[སྲོག་ཆགས་བཞི་པོ་དེ་ཚོ་དང་དོ་དམ་པ་ཉི་ཤུ་རྩ་བཞི་དེ་ཚོས་] (དེ་སྔ་རྩ་བ་ནས་བླངས་མ་མྱོང་བའི་) གླུ་གཞས་གསར་པ་ཞིག་བླངས་སོང་&#xf0d;&emsp;ཁོང་ཚོས་བླངས་པའི་གླུའི་ཚིག་དེ་འདི་ཚོ་རེད་འདུག:&ensp;"}
                                  {:line-no 211, :line "> །ཁྱེད་རང་ཤོག་དྲིལ་དེ་བླང་ནས་དེའི་ཐེལ་ཙེ་དེ་ཚོ་གཅག་ནས་ཁ་འབྱེད་འོས་པ་འདུག"}
                                  {:line-no 212, :line "> །གང་ཡིན་ཟེར་ན་ཁྱེད་རང་བསད་པ་དང་།&emsp;ཁྱེད་རང་གི་ཟུང་ཁྲག་"}
                                  {:line-no 213, :line "> དེས་འཛམ་བུ་གླིང་ཡོངས་རྫོགས་ཀྱི་མི་ཚོ་བླུ་ཉོ་བྱས་པ་རེད།"}
                                  {:line-no 214, :line "> ད་ལྟ་(མི་རིགས་དང་ལུང་པ་མི་གཅིག་པའི་མི་ཚང་མ་) དཀོན་མཆོག་ལ་བདག་གི་ཡོད་རེད།"}],
                          :type :verse,
                          :source "Himlit",
                          :verse-nos [9],
                          :lang "bo",
                          :chapter "5",
                          :chapter-no 5,
                          :verse-number "9"})

(def expect-add-verse-l2 {:index 291,
                          :book "Revelation",
                          :lines [{:line-no 291, :line "> <span class=\"vn\">6</span>12,000 were from the tribe of Asher;"}
                                  {:line-no 292, :line "> 12,000 were from the tribe of Naphtali; "}
                                  {:line-no 293, :line "> 12,000 were from the tribe of Manasseh."}],
                          :type :h4,
                          :source "Himlit",
                          :verse-nos [6],
                          :lang "english",
                          :chapter "7",
                          :chapter-no 7,
                          :verse-number "6"})

(deftest test-add-verse
         (is (= (attach-verse-number  add-verse-l1) expect-add-verse-l1))
         (is (= (attach-verse-number  add-verse-l2) expect-add-verse-l2)))



