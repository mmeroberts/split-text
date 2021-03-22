(ns split-text.core
  (:require
    [split-text.io :as spio]
    ;[clojure.java.io :as io]
    [split-text.markdown :as md]
    [split-text.config :as conf]
    [split-text.markdownout :as mdo]
    [clojure.string :as str]
    [clojure.tools.cli :as cli]
    [clojure.java.shell :as sh])
  (:gen-class))



(def cli-options
  ;; An option with a required argument
  [["-f" "--file FILENAME" "Filename without path or extension"]
   ["-d" "--directory DIRECTORY" "Directory that contains the working folders"]
   ["-c" "--config CONFIG" "Config file name"]
   ["-t" "--title TITLE" "The Title of the document"]
   ["-s" "--source SOURCE" "Source of Document" :default "Himlit"]
   ["-T" "--Test" "Test Dry Run"]
   ["-F" "--format FORMAT" "The format of the output document"
    :default "normal"
    :validate [#(contains? #{"normal" "parallel"} % ) "Must be either normal or parallel"]]
   ["-n" "--navigation" "Use navigation"]

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
        "Output Style:"
        "  bo         Print tibetan - will include english titles"
        "  eng        Print english text"
        "  web        Print World English Bible translation text"
        "  bowy       Print tibetan and wylie transliteration"
        "  back       Print back translation - will include english titles and headers"
        "  boeng      Print tibetan and english "
        "  boweb      Print tibetan and World English Bible translation "
        "  boback     Print tibetan and back translation "
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
           (#{"bo" "eng" "web" "bsb" "back" "bowy"  "bobsb" "boeng"  "boweb" "boback"} (first arguments)))
      {:style (first arguments) :options options}
      (or (:title options) (:config options)) ; catch all check - titlemust be defined.
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
  (let [{:keys [file book directory source]} (conf/get-config options)
        docfile (construct-filename directory  conf/original  file ".doc")
        docxfile (construct-filename directory  conf/intermediate  file ".docx")
        markdownfile (construct-filename directory  conf/intermediate  file ".in.md")
        create-docx (or (not (spio/file-exists? docxfile)) (and (spio/file-exists? docxfile)
                                                                (> (spio/lastModified docfile) (spio/lastModified docxfile))))
        create-markdown (or (not (spio/file-exists? markdownfile))
                            (and (spio/file-exists? markdownfile)
                                 (> (spio/lastModified docxfile) (spio/lastModified markdownfile))))]
    (when (not= exit 0)
      (println  create-docx ", " create-markdown ".")
      (if create-docx
             (do (sh/sh "doc2docx.bat" docfile docxfile)
                 (sh/sh "docx2md.bat" docxfile markdownfile))
             (when create-markdown (sh/sh "docx2md.bat" docxfile markdownfile)))
      (md/process-markdown-file markdownfile book source))))



(defn get-suffix [suffix navigation format]
  (let [nav (if (nil? navigation) "" "-nav")
        form (if (= format "normal") "" (str "-" format))]
    (str suffix nav form)))

(defn output-document [style options]
  (tap> (str options))
  (let [{:keys [file title book directory navigation format output]} (conf/get-config options)
        suffix (cond (= style "bo")  (get-suffix "uniglot" navigation format)
                     (= style "eng")  (get-suffix "english" navigation format)
                     (= style "web")  (get-suffix "web" navigation format)
                     (= style "bsb")  (get-suffix "bsb" navigation format)
                     (= style "back")  (get-suffix "back" navigation format)
                     (= style "bowy") (get-suffix "diglot-wylie" navigation format)
                     (= style "boeng") (get-suffix "diglot" navigation format)
                     (= style "boweb") (get-suffix "diglot-web" navigation format)
                     (= style "bobsb") (get-suffix "diglot-bsb" navigation format)
                     (= style "boback") (get-suffix "diglot-back" navigation format))
        outputfile (construct-filename directory  conf/pre-published file (str "-" suffix "." output))
        title-out (str title "-" suffix)]
    (when (not= exit 0)
      (case style
        "bo" (spio/output-html (mdo/output-book book "Himlit" "bo" nil nil format) title-out outputfile)
        "eng" (spio/output-html (mdo/output-book book "Himlit" "english" nil nil format) title-out outputfile)
        "web" (spio/output-html (mdo/output-book book "WEB" "english" nil nil format) title-out outputfile)
        "bsb" (spio/output-html (mdo/output-book book "BSB" "english" nil nil format) title-out outputfile)
        "back" (spio/output-html (mdo/output-book book "Himlit" "back" nil nil format) title-out outputfile)
        "bowy" (spio/output-html (mdo/output-book book "Himlit" "bo" "Himlit" "wylie" format) title-out outputfile)
        "boeng" (spio/output-html (mdo/output-book book "Himlit" "bo" "Himlit" "english" format) title-out outputfile)
        "boweb" (spio/output-html (mdo/output-book book "Himlit" "bo" "WEB" "english" format) title-out outputfile)
        "bobsb" (spio/output-html (mdo/output-book book "Himlit" "bo" "BSB" "english" format) title-out outputfile)
        "boback" (spio/output-html (mdo/output-book book "Himlit" "bo" "Himlit" "back" format) title-out outputfile)))))

(defn -main [& args]
  (let [{:keys [style options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (if (nil? style)
          (handle-document options)
          (output-document style options)))))

(comment
  ;(conf/get-config {:config "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation\\config\\config.edn" :source "Himlit"})
  ;; read in
  (-main "-c" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation\\config\\config.edn")
  ;(-main "-c" "C:\\Users\\MartinRoberts\\Sync\\GoodNewsForYou\\Book1\\config\\config.edn" "-T")
  ;(-main "-f" "2020-Revelation-Final" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation")
  ;(-main "-f" "Mark" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Gospels\\2020-ERV-Mark" "-t" "Mark" "-s" "Himlit")
  ;; output
  ;(-main "-f" "2020-Revelation-Final" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-t" "Revelation" "bo")
  ;(-main "-f" "2020-Revelation-Final" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-t" "Revelation" "eng")
  ;(-main "-f" "2020-Revelation-Final" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-t" "Revelation" "web")
  ;(-main "-f" "2020-Revelation-Final" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-t" "Revelation" "back")
  ;(-main "-f" "2020-Revelation-Final" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-t" "Revelation" "boeng")
  ;(-main "-f" "2020-Revelation-Final" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-t" "Revelation" "boback")
  (-main "-c" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation\\config\\config.edn" "bobsb")
  ;; output with navigation
  ;(-main "-f" "2020-Revelation-Final" "-n" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-t" "Revelation" "bo")
  ;; output with format no navigation
  ;(-main "-f" "2020-Revelation-Final" "-F" "parallel" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-t" "Revelation" "boweb")
  ;(-main "-f" "2020-Revelation-Final" "-F" "normal" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-t" "Revelation" "boweb")
  ;; output with format and navigation
  ;(-main "-f" "2020-Revelation-Final" "-n" "-F" "parallel" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-t" "Revelation" "boeng")
  ;(-main "-f" "2020-Revelation-Final" "-n" "-F" "normal" "-d" "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation" "-t" "Revelation" "boeng")
  ;
  ;
  ;
  ;(def dir "C:\\Users\\MartinRoberts\\Sync\\NT\\Revelation")
  ;;(def dir "C:\\Users\\MartinRoberts\\Sync\\NT\\Gospels\\2020-ERV-Mark")
  ;(def filein "Revelation-test")
  ;;(def filein "2020-Revelation-Final")
  ;;(def filein "Mark")
  ;(def mdf (construct-filename dir  conf/intermediate  filein ".in.md"))
  ;(def markdownfile (construct-filename dir  conf/intermediate  filein ".in.md"))
  ;(def docfile (construct-filename dir  conf/original  filein ".doc"))
  ;(def docxfile (construct-filename dir  conf/intermediate  filein ".docx"))
  ;(def mdcin (md/read-markdown mdf))
  ;(tap> mdcin)
  ;(def mdcproc (md/process-markdown-1 mdcin))
  ;(tap> mdcproc)
  ;(def mdcg (group-by :lang mdcproc))
  ;(def mdcgeng (:english mdcg))
  ;(def mdceng2 (md/process-markdown-2 mdcgeng))
  ;(def mdcproc1 (md/process-markdown-file mdf "Revelation" "Himlit1"))
  ;(db/add_entries db/conn "Himlit" "Revelation" mdcproc)
  ;(def mdcout (mdo/output-markdown "bo" mdcproc))
  ;(def intermediatefilename (construct-filename dir  conf/intermediate  filein ".out.md"))
  ;(def intermediateengfilename (construct-filename dir  conf/intermediate  filein "eng.out.md"))
  ;(def intermediateintfilename (construct-filename dir  conf/intermediate  filein "int.out.md"))
  ;(def mdout (mdo/output-md (mdo/output-bo-markdown mdcproc) intermediatefilename))
  ;
  ;(def outputfile (construct-filename dir  conf/pre-published filein (str "-" "uniglot" ".html")))
  ;(def outputengfile (construct-filename dir  conf/pre-published filein (str "-" "english" ".html")))
  ;(def outputdigfile (construct-filename dir  conf/pre-published filein (str "-" "diglot" ".html")))
  ;(def outputparafile (construct-filename dir  conf/pre-published filein (str "-" "diglot-parallel" ".html")))
  ;(def outputparawebfile (construct-filename dir  conf/pre-published filein (str "-" "diglot-web-parallel" ".html")))
  ;(def outputbowebfile (construct-filename dir  conf/pre-published filein (str "-" "diglot-web" ".html")))
  ;;(def mdout (mdo/output-md (mdo/output-bo-markdown "Himlit" "Revelation") intermediatefilename))
  ;(def mdouteng (mdo/output-md (mdo/output-eng-markdown "Himlit" "Revelation") intermediateengfilename))
  ;;(def mdouteng (mdo/output-md (mdo/output-eng-markdown "WEB" "Revelation") intermediateengfilename))
  ;(sh/sh "md2out.bat" intermediatefilename outputfile  "Revelation" conf/stylesheet)
  ;(sh/sh "md2out.bat" intermediateengfilename outputengfile  "Revelation" conf/stylesheet)
  ;(def mdoutint (mdo/output-md (mdo/output-boweb-markdown "Revelation") intermediateintfilename))
  ;(time(def mdoutint (mdo/output-md (mdo/output-boeng-markdown "Revelation") intermediateintfilename)))
  ;(sh/sh "md2out.bat" intermediateintfilename outputdigfile  "Revelation" conf/stylesheet)
  ;
  ;
  ;(mdo/output-html (mdo/output-bo-markdown "Himlit" "Mark")  "Mark" outputfile)
  ;(mdo/output-html (mdo/output-boeng-markdown "Revelation") "Revelation" outputdigfile)
  ;(mdo/output-html (mdo/output-boweb-markdown "Revelation") "Revelation" outputbowebfile)
  ;(mdo/output-html (mdo/output-boeng-parallel "Revelation") "Revelation" outputparafile)
  ;(mdo/output-html (mdo/output-boweb-parallel "Revelation") "Revelation" outputparawebfile)
  ;(mdo/output-html (mdo/output-boweb-parallel "Mark") "Revelation" outputparawebfile)
  ;{:vlaaad.reveal/command '(clear-output)}
  ;(System/currentTimeMillis)




  ,)

; (-main "-f" "James.doc" "boeng-cols")