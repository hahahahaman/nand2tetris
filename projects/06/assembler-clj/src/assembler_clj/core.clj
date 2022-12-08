(ns assembler-clj.core
  (:import java.io.File
           java.io.IOException)
  (:require [clojure.java.io :as io]
            [clojure.string :as s])
  (:gen-class))

;; comments, anything after //
;; things in brackets () ? ignore?
;; read .asm from args
;; output .hack file

;; https://www.tutorialspoint.com/clojure/clojure_file_io.htm
(comment

  ;; how to read a file?
  ;; read line by line
  (with-open [rdr (io/reader "README.md")]
    (vec (line-seq rdr)))

  ;; read as entire string
  (slurp "../add/Add.asm")

  ;; how to write to a file?
  ;; write line by line
  (with-open [w (io/writer "Example.txt" :append true)]
    (.write w (str "hello" "world")))

  ;; write entire string
  (spit "Example.txt"
        "hello"))

(defn usage []
  (println (->>
            ["assembler-clj - Assembler for the Hack machine"
             "Converts assembly files, .asm files, to binary code, .hack files"
             "Chapter 6 project for Elements of Computing Systems"
             ""
             "Usage:"
             " Assembler Prog.asm "]
            (s/join \newline))))

(defn -main [& args]
  (cond
    (nth args 0)
    (let [split-filename (s/split (nth args 0) #"\.")
          name (first split-filename)
          hack-filename (str name ".hack")]
      )

    :else
    (usage)))
