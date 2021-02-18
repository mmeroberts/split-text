(ns split-text.core
  (:require [split-text.config :refer [directory original intermediate pre-published stylesheet]]
    [split-text.io :refer :all]
    ;[split-text.meta :refer :all]
    ;[split-text.inwards :refer :all]
    ;[split-text.outwards :refer :all]
    [clojure.java.io :as io]
    [split-text.markdown :refer :all]
    [split-text.config :refer :all]
    [split-text.markdownout :refer :all]
    [split-text.db :refer :all]
    [clojure.string :as str]
    [com.rpl.specter :refer :all]
    [hiccup2.core :as h]
    [clojure.tools.cli :as cli]
    [clojure.java.shell :as sh]
    [crux.api :as crux])
  (:gen-class))


(def cli-options
  ;; An option with a required argument
  [["-f" "--file FILENAME" "Filename without path or extension"]
   ["-d" "--directory DIRECTORY" "Directory that contains the working folders"]
   ["-t" "--title TITLE" "The Title of the document"]
   ["-s" "--source SOURCE" "Source of Document" :default "Himlit"]
   ["-T" "--Test" "Test Dry Run" ]

   ; :validate [#(str/ends-with? % ".doc") "Must be a Word .doc file"]]
   ;; A non-idempotent option (:default is applied first)
   ["-o" "--output OUTPUT"  "Output format - options html or pdf"
    :default "html"
    :validate [#(contains? #{"html" "pdf"} % ) "Must be either html or pdf"]]
   ; Prior to 0.4.1, you would have to use:
   ;; :assoc-fn (fn [m k _] (update-in m [k] inc))
   ;; A boolean option defaulting to nil
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Format documents"
        ""
        "Usage: program-name [options] "
        ""
        "Options:"
        options-summary
        ""
        ;"Style:"
        ;"  bo         Print tibetan - will include english titles"
        ;"  eng        Print english text"
        ;"  back       Print back translation - will include english titles and headers"
        ;"  boeng      Print tibetan and english interleaved"
        ;"  boeng-cols  Print tibetan and english in columns"
        ;"  boback     Print tibetan and back translation interleaved"
        ;"  all        Print full document - all languages"
        ""
        "Please refer to the manual page for more information."]
       (str/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (str/join \newline errors)))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with a error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (:help options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}
      errors ; errors => exit with description of errors
      {:exit-message (error-msg errors)}
      ;; custom validation on arguments

      (and (= 1 (count arguments))
           (#{"bo" "bo-nav" "eng" "back" "boeng" "boeng-nav" "boeng-cols" "boback" "all"} (first arguments)))
      {:style (first arguments) :options options}
      (:title options) ; catch all check - titlemust be defined.
      {:options options}
      :else ; failed custom validation => exit with usage summary
      {:exit-message (usage summary)})))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn construct-filename [ d s f e]
  (str  d "\\" s "\\" f e))

(defn handle-document [ options]
  (tap> (str options))
  (let [{:keys [file title directory source]} options
        ;filestub (first (str/split file #"\."))
        ;suffix (cond (= style "bo")  "uniglot"
        ;             (= style "eng")  "english"
        ;             (= style "bo-nav")  "uniglot-nav"
        ;             (= style "boeng") "diglot-interlinear"
        ;             (= style "boeng-nav") "diglot-interlinear-nav"
        ;             (= style "boeng-cols") "diglot-side-by-side")

        docfile (construct-filename directory  original  file ".doc")
        docxfile (construct-filename directory  intermediate  file ".docx")
        markdownfile (construct-filename directory  intermediate  file ".in.md")
        intermediatefilename (construct-filename directory  intermediate  file ".out.md")
        ;outputfile (construct-filename directory  pre-published file (str "-" suffix "." output))
        create-docx (or (not (file-exists? docxfile)) (and (file-exists? docxfile)
                                                               (> (lastModified docfile) (lastModified docxfile))))
        create-markdown (or (not (file-exists? markdownfile))
                                (and (file-exists? markdownfile)
                                 (> (lastModified docxfile) (lastModified markdownfile))))]
    (if (not= exit 0)
        (do(println  create-docx ", " create-markdown ".")
           (if create-docx
             (do (sh/sh "doc2docx.bat" docfile docxfile)
                 (sh/sh "docx2md.bat" docxfile markdownfile))
             (if create-markdown (sh/sh "docx2md.bat" docxfile markdownfile)))
           (process-markdown-file markdownfile title source)
           )))
  )
                ;(case style
                ;  "bo" (output-md (output-bo-markdown md) intermediatefilename)
                ;  "eng" (output-md (output-eng-markdown md) intermediatefilename)
                ;  "bo-nav" (output-md-with-navigation (output-bo-markdown md) md style intermediatefilename)
                ;  "boeng" (output-md (output-boeng-markdown md) intermediatefilename)
                ;  "boeng-nav" (output-md-with-navigation (output-boeng-markdown md) md style intermediatefilename)
                ;  "boeng-cols" (output-md (output-boeng-interlinear md) intermediatefilename))
                ;(println "md2out.bat " intermediatefilename " " outputfile " " title " " stylesheet)
                ;(sh/sh "md2out.bat" intermediatefilename outputfile title stylesheet))

(defn output-document [style options]
  (tap> (str options))
  (let [{:keys [file title directory source]} options
        ;filestub (first (str/split file #"\."))
        suffix (cond (= style "bo")  "uniglot"
                     (= style "eng")  "english"
                     (= style "bo-nav")  "uniglot-nav"
                     (= style "boeng") "diglot-interlinear"
                     (= style "boeng-nav") "diglot-interlinear-nav"
                     (= style "boeng-cols") "diglot-side-by-side")

        docfile (construct-filename directory  original  file ".doc")
        docxfile (construct-filename directory  intermediate  file ".docx")
        markdownfile (construct-filename directory  intermediate  file ".in.md")
        intermediatefilename (construct-filename directory  intermediate  file ".out.md")
        ;outputfile (construct-filename directory  pre-published file (str "-" suffix "." output))
        create-docx (not(or (not (file-exists? docxfile)) (and (file-exists? docxfile)
                                                               (> (lastModified docfile) (lastModified docxfile)))))
        create-markdown (not(or (not (file-exists? markdownfile))
                                (and (file-exists? markdownfile)
                                     (> (lastModified docxfile) (lastModified markdownfile)))))]
    (if (not= exit 0)
      (do (println  create-docx ", " create-markdown ".")
        (if create-docx
          (do (sh/sh "doc2docx.bat" docfile docxfile)
              (sh/sh "docx2md.bat" docxfile markdownfile)))
          (if create-markdown  (sh/sh "docx2md.bat" docxfile markdownfile)))))
          ;(process-markdown-file markdownfile title source))))
  )
;(case style
;  "bo" (output-md (output-bo-markdown md) intermediatefilename)
;  "eng" (output-md (output-eng-markdown md) intermediatefilename)
;  "bo-nav" (output-md-with-navigation (output-bo-markdown md) md style intermediatefilename)
;  "boeng" (output-md (output-boeng-markdown md) intermediatefilename)
;  "boeng-nav" (output-md-with-navigation (output-boeng-markdown md) md style intermediatefilename)
;  "boeng-cols" (output-md (output-boeng-interlinear md) intermediatefilename))
;(println "md2out.bat " intermediatefilename " " outputfile " " title " " stylesheet)
;(sh/sh "md2out.bat" intermediatefilename outputfile title stylesheet))




(defn -main [& args]

  (let [{:keys [style options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (handle-document options))))

(comment
  (-main "-f" "Revelation-test" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-t" "Revelation1" "bo")
  (-main "-f" "Revelation-test" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-t" "Revelation" "-s" "Himlit")
  (-main "-f" "2020-Revelation-Final" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-t" "Revelation" "-s" "Himlit")
  (-main "-f" "Revelation-test" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-t" "Revelation")
  (-main "-t" "Revelation" "bo")
  (-main "-f" "Revelation-test" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-x" "-m" "-t" "Revelation" "bo")
  (-main "-f" "2020-Revelation-Final" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-x" "-m" "-t" "Revelation" "bo")
  (-main "-f" "2020-Revelation-Final" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-t" "Revelation")
  (-main "-f" "2020-Revelation-letters" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-x" "-m" "-t" "Revelation Letters" "bo")
  (-main "-f" "2020-Revelation-final" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-x" "-m" "-t" "Revelation" "boeng")
  (-main "-f" "2020-Revelation-letters" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-x" "-m" "-t" "Revelation Letters" "boeng")
  (def dir "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation")
  (def filein "Revelation-test")
  (def filein "2020-Revelation-Final")
  (def mdf (construct-filename dir  intermediate  filein ".in.md"))
  (def markdownfile (construct-filename dir  intermediate  filein ".in.md"))
  (def docfile (construct-filename dir  original  filein ".doc"))
  (def docxfile (construct-filename dir  intermediate  filein ".docx"))
  (def mdcin (read-markdown mdf))
  (def mdcproc (process-markdown-1 mdcin))
  (def mdcg (group-by :lang mdcproc))
  (def mdcgeng (:english mdcg))
  (def mdceng2 (process-markdown-2 mdcgeng))
  (def mdcproc1 (process-markdown-file mdf "Revelation" "Himlit1"))
  (add_entries conn "Himlit" "Revelation" mdcproc)
  (def mdcout (output-markdown "bo" mdcproc))
  (def intermediatefilename (construct-filename dir  intermediate  filein ".out.md"))
  (def intermediateengfilename (construct-filename dir  intermediate  filein "eng.out.md"))
  (def intermediateintfilename (construct-filename dir  intermediate  filein "int.out.md"))
  (def mdout (output-md (output-bo-markdown mdcproc) intermediatefilename))

  (def outputfile (construct-filename dir  pre-published filein (str "-" "uniglot" ".html")))
  (def outputengfile (construct-filename dir  pre-published filein (str "-" "english" ".html")))
  (def outputdigfile (construct-filename dir  pre-published filein (str "-" "diglot" ".html")))
  (sh/sh "md2out.bat" intermediatefilename outputfile  "Revelation" stylesheet)
  (sh/sh "md2out.bat" intermediateengfilename outputengfile  "Revelation" stylesheet)

  (def mdoutint (output-md (output-boeng-markdown "Revelation") intermediateintfilename))
  (sh/sh "md2out.bat" intermediateintfilename outputdigfile  "Revelation" stylesheet)





  (loop [  [one two & remaining] m  o []]
    (println one two remaining o)
    (cond (and (= (:t one) :a) (= (:t two) :b) (recur ((comp vec flatten conj) [] [] (assoc one :t1 [(:t one) (:t two)]) (flatten remaining)) o))
          (and (nil? one) (nil? two)) o
          (nil? two) (let [n (assoc one :t1 [(:t one)])] (recur two (conj o n)))
          :else (let [n (if (not (contains? one :t1)) (assoc one :t1 [(:t one)]) one)](recur ((comp vec flatten conj) [] [] two remaining) (conj  o n)))))
  ,)

; (-main "-f" "James.doc" "boeng-cols")