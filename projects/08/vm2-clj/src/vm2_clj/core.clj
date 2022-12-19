(ns vm2-clj.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint]])
  (:gen-class))

(defn get-file-extension
  "File extension from the path string. Return value includes the period: .txt
  https://rosettacode.org/wiki/Extract_file_extension#Clojure"
  [path]
  (second (re-find #"(\.[a-zA-Z0-9]+)$" path)))

(comment
  (def symbol-address-table
    {"SP" 0
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
     "KBD" 24576}))

;; init addresses
(def SP 256)
(def LCL 300)
(def ARG 400)
(def THIS 3000)
(def THAT 3010)
(def TEMP 5)

(def init-pointers-asm
  ;; init stack pointer: M[0] = 256
  (s/join
   "\n"
   ["@256" ;; SP 256
    "D=A"
    "@SP" ;; stack pointer, at address 0. set A=0
    "M=D" ;; M[0]=256

    "@300" ;; LCL 300
    "D=A"
    "@LCL"
    "M=D"

    "@400" ;; ARG 400
    "D=A"
    "@ARG"
    "M=D"

    "@3000" ;; THIS 3000
    "D=A"
    "@THIS"
    "M=D"

    "@3010" ;; THAT 4000
    "D=A"
    "@THAT"
    "M=D"]))

(def inc-stack-pointer
  (s/join
   "\n"
   ["@SP"
    "M=M+1"]))

(def dec-stack-pointer
  (s/join
   "\n"
   ["@SP"
    "M=M-1"]))

;; (defn inc-pointer [pointer-sym]
;;   (s/join
;;    "\n"
;;    [(str "@" pointer-sym)
;;     "M=M+1"]))

;; (defn dec-pointer [pointer-sym]
;;   (s/join
;;    "\n"
;;    [(str "@" pointer-sym)
;;     "M=M-1"]))

(def D=SP ;; D register = value at address pointed to by SP
  (s/join
   "\n"
   ["@SP"
    "M=M-1"
    "A=M"
    "D=M"]))

(def SP=D ;; value at address pointed to by SP = D register
  (s/join
   "\n"
   ["@SP"
    "A=M"
    "M=D"]))

(def SP=SP+D
  (s/join
   "\n"
   ["@SP"
    "A=M"
    "M=M+D"]))

(defn D=v [v]
  (s/join
   "\n"
   [(str "@" v)
    "D=A"]))

(defn write-file! [filename lines]
  (println "Writing to:" filename)
  (with-open [w (io/writer filename :append false)]
    (doseq [l lines]
      (.write w (str l "\n"))
      ;; (println l)
      )))

(defn gen-stack-arithmetic-asm [op]
  (condp = op
    "add"
    (s/join "\n"
            [
             ;; "@SP" ;; A = 0            ;; M[0] = 258
             ;; "M=M-1" ;; M[0] = M[0] -1 ;; M[0] = 257
             ;; "A=M" ;; A = M[0]         ;; A = 257
             ;; "D=M" ;; D = M[0]         ;; D = M[257]
             D=SP

             ;; "@SP" ;; A = 0
             ;; "M=M-1" ;; M[0] = M[0]-1  ;; M[0] = 256
             dec-stack-pointer

             "A=M" ;; A = M[0]            ;; A = 256
             "M=M+D" ;; M[256]=M[256]+M[257]

             ;; "@SP"
             ;; "M=M+1" ;; M[0] = 257
             inc-stack-pointer])

    "sub"
    (s/join "\n" [D=SP

                  dec-stack-pointer

                  "A=M"
                  "M=M-D"

                  inc-stack-pointer])

    "neg"
    (s/join "\n" [dec-stack-pointer

                  "A=M"
                  "M=-M"

                  inc-stack-pointer])

    "and"
    (s/join "\n"
            [D=SP

             dec-stack-pointer

             "A=M"
             "M=M&D"


             inc-stack-pointer])

    "or"
    (s/join "\n" [D=SP

                  dec-stack-pointer

                  "A=M"
                  "M=M|D"

                  inc-stack-pointer])

    "not"
    (s/join "\n" [dec-stack-pointer

                  "A=M"
                  "M=!M"

                  inc-stack-pointer])))

(defn gen-comparison-asm! [op new-label!]
  (condp = op
    "eq"
    ;; -1 is true, 0 is false
    (let [{cond-sym :symbol cond-label :label} (new-label!)
          {end-sym :symbol end-label :label} (new-label!)]
      (s/join "\n" [D=SP

                    dec-stack-pointer

                    "A=M" ;; set A to the SP address
                    "D=M-D"

                    ;; where do i jump?
                    ;; i keep a symbol string with a incrementing
                    ;; suffix
                    ;; (LABEL1)
                    (str "@" cond-sym)
                    "D;JEQ" ;; jump to the cond-label

                    ;; false, set M[SP]=0
                    "@SP"
                    "A=M"
                    "M=0"
                    (str "@" end-sym)
                    "0;JMP"

                    ;; set SP to -1
                    cond-label
                    "@SP"
                    "A=M"
                    "M=-1"

                    end-label
                    inc-stack-pointer]))

    "gt"
    (let [{cond-sym :symbol cond-label :label} (new-label!)
          {end-sym :symbol end-label :label} (new-label!)]
      (s/join "\n" [D=SP

                    dec-stack-pointer

                    "A=M" ;; set A to the SP address
                    "D=M-D"
                    (str "@" cond-sym)
                    "D;JGT" ;; jump to the cond-label

                    ;; false, set M[SP]=0
                    "@SP"
                    "A=M"
                    "M=0"
                    (str "@" end-sym)
                    "0;JMP"

                    ;; set SP to -1
                    cond-label
                    "@SP"
                    "A=M"
                    "M=-1"

                    end-label
                    inc-stack-pointer]))

    "lt"
    (let [{cond-sym :symbol cond-label :label} (new-label!)
          {end-sym :symbol end-label :label} (new-label!)]
      (s/join "\n" [D=SP

                    dec-stack-pointer

                    "A=M" ;; set A to the SP address
                    "D=M-D"
                    (str "@" cond-sym)
                    "D;JLT" ;; jump to the cond-label

                    ;; false, set M[SP]=0
                    "@SP"
                    "A=M"
                    "M=0"
                    (str "@" end-sym)
                    "0;JMP"

                    ;; set SP to -1
                    cond-label
                    "@SP"
                    "A=M"
                    "M=-1"

                    end-label
                    inc-stack-pointer]))))

(defn gen-memory-access-asm [op seg idx filename]
  (condp = op

    ;; push s i; push the value of s[i] onto the stack
    "push"
    (str

     ;; first set the D-register to the value of s[i]
     (condp = seg
       "constant"
       (s/join
        "\n"
        [(str "@" idx) ;; put constant into D-register
         "D=A"])

       "local"
       (s/join
        "\n"
        [(str "@" (+ LCL idx))
         "D=M"])

       "argument"
       (s/join "\n" [(str "@" (+ ARG idx))
                     "D=M"])

       "this"
       (s/join "\n" ["@THIS"
                     "D=M" ;; THIS address
                     (str "@" idx)
                     "D=D+A"
                     "A=D" ;; A = THIS + idx

                     "D=M"])

       "that"
       (s/join "\n" ["@THAT"
                     "D=M" ;; THIS address
                     (str "@" idx)
                     "D=D+A"
                     "A=D" ;; A = THIS + idx

                     "D=M"])

       "temp"
       (s/join "\n" [(str "@" (+ TEMP idx))
                     "D=M"])

       "pointer"
       (condp = idx
         0
         (s/join
          "\n"
          ["@THIS"
           "D=M"])

         1
         (s/join
          "\n"
          ["@THAT"
           "D=M"])

         :else
         (println "ERROR: pointer index must be 0 or 1."))

       "static"
       (s/join
        "\n"
        [(str "@" filename "." idx)
         "D=M"])

       (println "ERROR: No matching push SEGMENT" seg))

     "\n"

     ;; second push onto stack
     (s/join
      "\n"
      [;; set value at the top of the stack to the in D-register value
       "@SP"
       "A=M"
       "M=D"

       ;; increment stack pointer
       "@SP"
       "M=M+1"]))

    ;; pop  s i; pop top of stack and store into s[i]
    "pop"
    (str

     ;; first get the address of s[i]
     ;; D=SP pops top of stack and places the value in D-register
     (condp = seg
       "local"
       (s/join "\n" [D=SP
                     (str "@" (+ LCL idx))])

       "argument"
       (s/join "\n" [D=SP
                     (str "@" (+ ARG idx))])

       "this"
       (s/join
        "\n"
        ;; use R13 to store the memory address of s[i]
        ;; pointer can change the the value of THIS at runtime
        ["@THIS"
         "D=M" ;; THIS address
         (str "@" idx)
         "D=D+A"
         "@R13"
         "M=D" ;; R13 = THIS + idx

         D=SP

         "@R13"
         "A=M" ;; A = THIS + idx
         ])

       "that"
       (s/join "\n" ["@THAT"
                     "D=M"
                     (str "@" idx)
                     "D=D+A"
                     "@R13"
                     "M=D" ;; R13 = THAT + idx

                     D=SP

                     "@R13"
                     "A=M" ;; A = THAT + idx
                     ])

       "temp"
       (s/join "\n" [D=SP
                     (str "@" (+ TEMP idx))])

       "pointer"
       (condp = idx
         0
         (s/join "\n" [D=SP
                       "@THIS"])

         1
         (s/join "\n" [D=SP
                       "@THAT"])

         :else
         (println "ERROR: pointer index must be 0 or 1."))

       "static"
       (s/join "\n" [D=SP
                     (str "@" filename "." idx)])

       (println "ERROR: No matching pop SEGMENT" seg))

     "\n"

     ;; second set value at memory address to D-register value
     "M=D"
     )))

(defn gen-program-flow-asm [op seg idx]
  (condp = op
    "label"
    (s/join "\n" [(str "(" seg ")")])

    "goto"
    (s/join "\n" [(str "@" seg)
                  "0;JMP"])

    "if-goto"
    (s/join "\n" [D=SP ;; pop top of stack
                  (str "@" seg)
                  "D;JNE" ;; jump if stack value != 0
                  ])

    "return"
    (s/join "\n" [;; TODO
                  "0;JMP"])
    ))

(defn gen-function-calling-asm [op seg idx]
  (condp = op
    "function"
    (s/join "\n" [
                  ;;TODO
                  ])

    "call"
    (s/join "\n" [
                  ;;TODO
                  ])))

(defn translate-vm-file! [file]
  (let [filepath (.getPath file)
        filename (.getName file)
        filename-without-ext (subs filename 0 (- (count filename)
                                                 (count ".vm")))
        asm-filepath (str (subs filepath 0 (- (count filepath)
                                              (count ".vm")))
                          ".asm")
        vm-label "VMLABEL"
        !label-counter (atom 0)
        new-label! (fn []
                     (let [symbol (str vm-label @!label-counter)
                           label (str "(" symbol ")")]
                       (swap! !label-counter inc)
                       {:label label
                        :symbol symbol}))

        !out (atom [init-pointers-asm])
        lines (with-open [rdr (io/reader file)]
                (doall (mapv s/trim (line-seq rdr))))]
    (loop [i 0]
      (when (< i (count lines))
        (let [line (nth lines i)
              [op seg idx-str] (s/split line #"\s")
              idx (when idx-str (parse-long idx-str))
              asm (cond
                    (contains? #{"add" "sub" "neg" "and" "or" "not"} op)
                    (gen-stack-arithmetic-asm op)

                    (contains? #{"eq" "gt" "lt"} op)
                    (gen-comparison-asm! op new-label!)

                    (contains? #{"push" "pop"} op)
                    (gen-memory-access-asm op seg idx filename-without-ext)

                    (contains? #{"label" "goto" "if-goto" "return"} op)
                    (gen-program-flow-asm op seg idx)

                    ;; comment or empty
                    (or (= op "") (= op "//" )) nil

                    :else
                    (println "Line" (inc i)
                             ". ERROR: No matching operation"
                             op))]

          (when asm (swap! !out conj asm)))

        (recur (inc i))))

    (write-file! asm-filepath @!out)))

(defn translate-files! [path files]
  (doseq [file files]
    (let [file? (.isFile file)
          extension (get-file-extension (.getName file))]
      (if (not file?)
        (println "Skipping" path "not a file.")

        (if (not= extension ".vm")
          (println "Skipping" path "not a .vm file.")

          (translate-vm-file! file))))))

(defn usage []
  (println
   (->>
    ["vm1-clj - VM-to-Hack Translator Part 1"

     "Translate stack arithmetic and memory access commands of the VM language
       to the Hack assembly"

     "---Chapter 7 project for Elements of Computing Systems---"

     ""

     "Usage:"
     " VMtranslator Prog.vm"]
    (s/join \newline))))

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

        (translate-files! path files)))

    :else
    (usage)))
