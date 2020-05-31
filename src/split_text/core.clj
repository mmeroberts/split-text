(ns split-text.core
  (:require [split-text.config :refer :all]
    [split-text.io :refer :all]
    ;[split-text.meta :refer :all]
    ;[split-text.inwards :refer :all]
    ;[split-text.outwards :refer :all]
    [split-text.markdown :refer :all]
    ;[crux.api :as crux]
    [clojure.string :as str]
    [com.rpl.specter :refer :all]
    [hiccup2.core :as h]
    [clojure.tools.cli :as cli]
    [clojure.java.shell :as sh])
  (:gen-class))


(def cli-options
  ;; An option with a required argument
  [["-f" "--file FILENAME" "Filename without path or extension"]
   ["-d" "--directory DIRECTORY" "Directory that contains the working folders"]
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
        "Usage: program-name [options] style"
        ""
        "Options:"
        options-summary
        ""
        "Style:"
        "  bo         Print tibetan - will include english titles"
        "  eng        Print english text"
        "  back       Print back translation - will include english titles and headers"
        "  boeng      Print tibetan and english interleaved"
        "  boeng-col  Print tibetan and english in columns"
        "  boback     Print tibetan and back translation interleaved"
        "  all        Print full document - all languages"
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
           (#{"bo" "eng" "back" "boeng" "boeng-cols" "boback" "all"} (first arguments)))
      {:style (first arguments) :options options}
      :else ; failed custom validation => exit with usage summary
      {:exit-message (usage summary)})))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn construct-filename [ d s f e]
  (str  d "\\" s "\\" f e))

(defn handle-document [style options]
  (let [{:keys [output file directory]} options
        ;filestub (first (str/split file #"\."))
        docfile (construct-filename directory  original  file ".doc")
        docxfile (construct-filename directory  intermediate  file ".docx")
        markdownfile (construct-filename directory  intermediate  file ".in.md")
        intermediatefilename (construct-filename directory  intermediate  file ".out.md")
        outputfile (construct-filename directory  pre-published file (str "." output))
        one (println "doc2docx.bat " docfile " " docxfile)
        {:keys [exit out err] }(sh/sh "doc2docx.bat" docfile docxfile)
        two (println "docx2md.bat " docxfile " " markdownfile)
        {:keys [exit out err] }(sh/sh "docx2md.bat" docxfile markdownfile)]

    (if (not= exit 0) (println err)
                      (do (case style
                            "bo" (output-md (output-bo-markdown (process-markdown-file markdownfile)) intermediatefilename)
                            "boeng" (output-md (output-boeng-markdown (process-markdown-file markdownfile)) intermediatefilename)
                            "boeng-cols" (output-md (output-boeng-interlinear (process-markdown-file markdownfile)) intermediatefilename))
                          (println "md2out.bat " intermediatefilename " " outputfile " " file " " stylesheet)
                          (sh/sh "md2out.bat" intermediatefilename outputfile  file stylesheet)))))





(defn -main [& args]
  (let [{:keys [style options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (handle-document style options))))

;(-main "-f" "James.doc" "boeng-cols")