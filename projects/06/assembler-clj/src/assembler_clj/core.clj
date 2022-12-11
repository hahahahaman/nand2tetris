(ns assembler-clj.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

;; comments, anything after //
;; things in brackets () ? ignore?
;; read .asm from args
;; !binary-output .hack file

;; 1. parsing
;; 2. code module to go from binary codes to assembly mnemonics
;; 3. symbol table to handle symbols

;; read and write files
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
             "---Chapter 6 project for Elements of Computing Systems---"
             ""
             "Usage:"
             " Assembler Prog.asm "]
            (s/join \newline))))

(def default-symbol-table {"SP" 0
                           "LCL" 1
                           "ARG" 2
                           "THIS" 3
                           "THAT" 4
                           "R0" 0
                           "R1" 1
                           "R2" 2
                           "R3" 3
                           "R4" 4
                           "R5" 5
                           "R6" 6
                           "R7" 7
                           "R8" 8
                           "R9" 9
                           "R10" 10
                           "R11" 11
                           "R12" 12
                           "R13" 13
                           "R14" 14
                           "R15" 15
                           "SCREEN" 16384
                           "KBD" 24576})

(defn -main [& args]
  (cond
    (nth args 0)
    (let [filepath (nth args 0)
          position-of-last-forwardslash (s/last-index-of filepath "/")
          path (subs filepath 0 (if position-of-last-forwardslash
                                  (inc position-of-last-forwardslash)
                                  0))
          filename (-> (subs filepath (count path))
                       (s/split #"\.") ;; split off file extension
                       (first) ;; get the filename
                       )
          hack-filename (str path filename ".hack")
          !symbol-table (atom default-symbol-table)
          !symbol-address-counter (atom 16)
          !binary-output (atom [])
          !instruction-counter (atom 0)]

      (with-open [rdr (io/reader filepath)]
        (let [lines ((comp
                      (partial filterv not-empty) ;; remove empty strs
                      (partial mapv s/trim) ;; trim first
                      )

                     (line-seq rdr))]

          ;; first pass generate symbol table
          (loop [i 0]
            (when (< i (count lines))
              (let [line (nth lines i)
                    commands (s/split line #" ")
                    command (first commands)]
                (cond
                  (empty? command) nil

                  ;; handle comment
                  (and (= (first command) \/ )
                       (= (second command) \/ ))
                  ;; (println "Comment line" i ", ignoring.")
                  nil

                  ;; a-instruction: @value
                  (= (first command) \@)
                  (swap! !instruction-counter inc)

                  ;; handle L-command: (Xxx)
                  (and (= (first command) \( )
                       (= (last command)  \) ))
                  (let [symbol (subs command 1 (dec (count command)))]
                    (when (not (contains? @!symbol-table symbol))
                      ;; associate the symbol with a new address
                      (swap! !symbol-table
                             assoc symbol @!instruction-counter)))

                  ;; C-instruction: dest=comp;jump
                  :else (swap! !instruction-counter inc)))
              (recur (inc i))))

          ;; second pass translate commands
          (loop [i 0]
            (when (< i (count lines))
              (let [line (nth lines i)
                    commands (s/split line #" ")
                    command (first commands)]
                (cond
                  (empty? command) nil

                  ;; handle comment
                  (and (= (first command) \/) (= (second command) \/) )
                  ;; (println "Comment line" i ", ignoring.")
                  nil

                  ;; handle A-instruction: @value
                  (= (first command) \@)
                  (let [symbol (subs command 1)
                        int (parse-long symbol)
                        symbol? (nil? int)

                        value
                        (if symbol?
                          (do
                            (when (not (contains? @!symbol-table symbol))
                              ;; associate the symbol with a new address
                              (swap! !symbol-table
                                     assoc symbol @!symbol-address-counter)
                              (swap! !symbol-address-counter inc))

                            ;; get the address
                            (get @!symbol-table symbol))

                          int)

                        binary-str (Integer/toBinaryString value)
                        padding-length (- 16 (count binary-str))
                        code (str (apply str (repeat padding-length "0"))
                                  binary-str)]
                    (swap! !binary-output conj code))

                  ;; handle L-command: (Xxx)
                  (and (= (first command) \( )
                       (= (last command)  \) ))
                  nil

                  :else
                  ;; maybe C-instruction: dest=comp;jump
                  ;; or unrecognized
                  (let [!dest-str (atom "")
                        !comp-str (atom "")
                        !jump-str (atom "")
                        !equal-sign-pos (atom -1)
                        !semicolon-pos (atom (count command))]

                    (loop [i 0]
                      (when (< i (count command))
                        (cond
                          (= (nth command i) \=)
                          (do
                            (reset! !equal-sign-pos i)
                            (reset! !dest-str
                                    (subs command 0 @!equal-sign-pos)))

                          (= (nth command i) \;)
                          (do
                            (reset! !semicolon-pos i)
                            (reset! !jump-str
                                    (subs command (inc @!semicolon-pos)))))

                        (recur (inc i))))

                    (reset! !comp-str
                            (subs command
                                  (inc @!equal-sign-pos)
                                  @!semicolon-pos))

                    ;; check that the strs are valid
                    (let [dest-binary-str
                          (condp = (s/lower-case @!dest-str)
                            "m" "001"
                            "d" "010"
                            "md" "011"
                            "a" "100"
                            "am" "101"
                            "ad" "110"
                            "amd" "111"

                            "" "000"

                            (do
                              (println "Line" i
                                       ": Invalid dest menumonic:" @!dest-str)
                              "000"))

                          comp-binary-str
                          (let [lc-comp-str (s/lower-case @!comp-str)
                                replace-m (s/replace lc-comp-str
                                                     \m \a)

                                comp-part (condp = replace-m
                                            "0"   "101010"
                                            "1"   "111111"
                                            "-1"  "111010"
                                            "d"   "001100"
                                            "a"   "110000"
                                            "!d"  "001101"
                                            "!a"  "110001"
                                            "-d"  "001111"
                                            "-a"  "110011"
                                            "d+1" "011111"
                                            "a+1" "110111"
                                            "d-1" "001110"
                                            "a-1" "110010"
                                            "d+a" "000010"
                                            "d-a" "010011"
                                            "a-d" "000111"
                                            "d&a" "000000"
                                            "d|a" "010101")
                                memory? (not= replace-m lc-comp-str)]
                            (str (if memory? "1" "0") comp-part))

                          jump-binary-str
                          (condp = @!jump-str
                            ""    "000"
                            "JGT" "001"
                            "JEQ" "010"
                            "JGE" "011"
                            "JLT" "100"
                            "JNE" "101"
                            "JLE" "110"
                            "JMP" "111"

                            (do
                              (println "Line" i
                                       ": Invalid jump menumonic:" @!jump-str)
                              "000")
                            )]
                      (cond
                        (and (not-empty dest-binary-str)
                             (not-empty comp-binary-str)
                             (not-empty jump-binary-str))
                        (do
                          (swap! !binary-output conj (str "111"
                                                          comp-binary-str
                                                          dest-binary-str
                                                          jump-binary-str)))

                        :else
                        (println "line " i ": Unrecognized: " command))))))
              (recur (inc i))))))

      (with-open [w (io/writer hack-filename :append false)]
        (doseq [s @!binary-output]
          (.write w (str s "\n"))
          ;; (println s)
          ))

      (println "Output to filepath:" hack-filename)
      ;; (println "Done!")
      ;; (println "SymbolTable:")
      ;; (pprint @!symbol-table)
      )

    :else
    (usage)))
