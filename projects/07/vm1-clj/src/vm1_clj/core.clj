(ns vm1-clj.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint]])
  (:gen-class))


;; TODO use java/clojure built-in checking system for directory or files
;; https://clojure-doc.org/articles/cookbooks/files_and_directories/
;; https://docs.oracle.com/javase/7/docs/api/java/io/File.html

;; https://rosettacode.org/wiki/Extract_file_extension#Clojure
(defn get-file-extension [path]
  (second (re-find #"(\.[a-zA-Z0-9]+)$" path)))

(defn -main [& args]
  (cond
    ;; go through each line of the .vm file and out the corresponding assembly
    ;; if the path is a directory, process all .vm files in the directory
    (nth args 0)
    (let [path (io/file (s/trim (nth args 0)))
          exists? (.exists path)
          dir? (.isDirectory path)
          files (if dir? (vec (file-seq path)) [path])]
      (if (not exists?)
        (println "ERROR:" path " does not exist!")

        (doseq [file files]
          (let [file? (.isFile file)
                filename (.getName file)
                extension (get-file-extension (.getName file))]
            (if file?
              (if (= extension ".vm")
                (do
                  ;; parse
                  )
                (println "Skipping" path "not a .vm file."))
              (println "Skipping" path "not a file."))))))

    :else
    (println
     (->>
      ["vm1-clj - VM-to-Hack Translator Part 1"

       "Translate stack arithmetic and memory access commands of the VM language
       to the Hack assembly"

       "---Chapter 7 project for Elements of Computing Systems---"

       ""

       "Usage:"
       " VMtranslator Prog.vm"]
      (s/join \newline)))
    )
  )
