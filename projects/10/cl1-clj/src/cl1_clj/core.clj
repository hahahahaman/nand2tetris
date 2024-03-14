(ns cl1-clj.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint]])
  (:import java.io.File)
  (:gen-class))

(defn get-file-extension
  "File extension from the path string. Return value includes the period: .txt
  https://rosettacode.org/wiki/Extract_file_extension#Clojure"
  [path]
  (second (re-find #"(\.[a-zA-Z0-9]+)$" path)))

(defn conj-colls
  "conj all collections together within a collection

  Example:
  (conj-colls (list [1 2 3] [4 5 6] [7 8 9])) => [1 2 3 4 5 6 7 8 9]"
  [colls]
  (reduce (fn [a b] (apply conj a b)) colls))

(defn write-file! [filename lines]
  (println "Writing to:" filename)
  (with-open [w (io/writer filename :append false)]
    (doseq [l lines]
      (.write w (str l "\n"))
      ;; (println l)
      )))

(defn -main [& args]
  (cond
    (nth args 0)
    (let [file-or-dir (io/file (s/trim (nth args 0)))
          exists? (.exists file-or-dir)]
      (if (not exists?)
        (println "ERROR:" file-or-dir " does not exist!")

        (let [dir? (.isDirectory file-or-dir)

              files (if dir? (vec (file-seq file-or-dir)) [file-or-dir])

              filename (.getName file-or-dir)
              file-extension (get-file-extension filename)
              path (.getPath file-or-dir)
              out-filepath (if dir?
                             (str path "/" filename ".vm")
                             (when (= file-extension ".jack")
                               (str
                                (subs path 0 (- (count path)
                                                (count ".jack")))
                                ".vm")))]
          (doseq [file files]
            (if-not (.isFile file)
              (println "Skipping" path ", not a file.")

              (let [path (.getPath file)
                    filename (.getName file)
                    extension (get-file-extension filename)
                    filename-without-ext (subs filename
                                               0
                                               (- (count filename)
                                                  (count extension)))]
                (if-not (= extension ".jack")
                  (println "Skipping" path ", not a .jack file.")

                  (with-open [rdr (io/reader file)]
                    (doseq [line (line-seq rdr)]
                      (let [tokens (s/split line #"\s")]
                        )))))))

          )))

    :else
    (println
     (->>
      ["cl1-clj - Jack-to-VM Translator Part 1"

       "Translate Jack programs in to stack arithmetic and memory access commands of the VM language"

       "---Chapter 10 project for Elements of Computing Systems---"
       ]
      (s/join \newline)))
    ))
