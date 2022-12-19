(ns vm1-clj.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint]])
  (:gen-class))


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

(defn get-file-extension
  "File extension from the path string. Return value includes the period: .txt
  https://rosettacode.org/wiki/Extract_file_extension#Clojure"
  [path]
  (second (re-find #"(\.[a-zA-Z0-9]+)$" path)))

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

(defn inc-pointer [pointer-sym]
  (s/join
   "\n"
   [(str "@" pointer-sym)
    "M=M+1"]))

(defn dec-pointer [pointer-sym]
  (s/join
   "\n"
   [(str "@" pointer-sym)
    "M=M-1"]))

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

(defn -main [& args]
  (cond
    ;; go through each line of the .vm file and out the corresponding assembly
    ;; if the path is a directory, process all .vm files in the directory
    (nth args 0)
    (let [path (io/file (s/trim (nth args 0)))
          exists? (.exists path)
          dir? (.isDirectory path)
          files (if dir? (vec (file-seq path)) [path])
          !stack (atom [])]
      (if (not exists?)
        (println "ERROR:" path " does not exist!")

        (doseq [file files]
          (let [file? (.isFile file)
                extension (get-file-extension (.getName file))
                !label-counter (atom 0)
                vm-label "VMLABEL"
                new-label! (fn []
                             (let [symbol (str vm-label @!label-counter)
                                   label (str "(" symbol ")")]
                               (swap! !label-counter inc)
                               {:label label
                                :symbol symbol}))

                SP 256
                LCL 300
                ARG 400
                THIS 3000
                THAT 3010
                TEMP 5

                ;; set sp 256,        // stack pointer
                ;; set local 300,     // base address of the local segment
                ;; set argument 400,  // base address of the argument segment
                ;; set this 3000,     // base address of the this segment
                ;; set that 3010,     // base address of the that segment
                !out (atom
                      ;; init stack pointer: M[0] = 256
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
                       "M=D"]

                      )]
            (if file?
              (if (= extension ".vm")
                (let [lines (with-open [rdr (io/reader file)]
                              (doall (mapv s/trim (line-seq rdr))))]
                  (loop [i 0]
                    (when (< i (count lines))
                      (let [line (nth lines i)
                            [op seg idx-str] (s/split line #"\s")
                            idx (when idx-str (parse-long idx-str))]

                        ;; stack arithmetic commands
                        ;; memory access commands
                        (condp = op
                          "add"
                          (swap! !out conj

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
                                 inc-stack-pointer)

                          "sub"
                          (swap! !out conj
                                 D=SP

                                 dec-stack-pointer

                                 "A=M"
                                 "M=M-D"

                                 inc-stack-pointer)

                          "neg"
                          (swap! !out conj
                                 dec-stack-pointer

                                 "A=M"
                                 "M=-M"

                                 inc-stack-pointer)

                          "eq"
                          ;; -1 is true, 0 is false
                          (let [{cond-sym :symbol cond-label :label} (new-label!)
                                {end-sym :symbol end-label :label} (new-label!)]
                            (swap! !out conj

                                   D=SP

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
                                   inc-stack-pointer))

                          "gt"
                          (let [{cond-sym :symbol cond-label :label} (new-label!)
                                {end-sym :symbol end-label :label} (new-label!)]
                            (swap! !out conj
                                   D=SP

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
                                   inc-stack-pointer))

                          "lt"
                          (let [{cond-sym :symbol cond-label :label} (new-label!)
                                {end-sym :symbol end-label :label} (new-label!)]
                            (swap! !out conj
                                   D=SP

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
                                   inc-stack-pointer))

                          "and"
                          (swap! !out conj
                                 D=SP

                                 dec-stack-pointer

                                 "A=M"
                                 "M=M&D"


                                 inc-stack-pointer)

                          "or"
                          (swap! !out conj
                                 D=SP

                                 dec-stack-pointer

                                 "A=M"
                                 "M=M|D"

                                 inc-stack-pointer)

                          "not"
                          (swap! !out conj
                                 dec-stack-pointer

                                 "A=M"
                                 "M=!M"

                                 inc-stack-pointer)


                          ;; 0. You have already handled the constant segment.
                          ;;
                          ;; 1. Next, handle the segments local, argument, this,
                          ;; and that.
                          ;;
                          ;; 2. Next, handle the pointer and temp segments, in
                          ;; particular allowing modiﬁcation of the bases of
                          ;; the this and that segments.
                          ;;
                          ;; 3. Finally, handle the static segment.

                          ;; push s i; push the value of s[i] onto the stack
                          "push"
                          (do
                            (condp = seg
                              "constant"
                              (swap! !out conj
                                     ;; put constant into D-register
                                     (str "@" idx)
                                     "D=A")

                              "local"
                              (swap! !out conj
                                     (str "@" (+ LCL idx))
                                     "D=M")

                              "argument"
                              (swap! !out conj
                                     (str "@" (+ ARG idx))
                                     "D=M")

                              "this"
                              (swap! !out conj
                                     "@THIS"
                                     "D=M" ;; THIS address
                                     (str "@" idx)
                                     "D=D+A"
                                     "A=D" ;; A = THIS + idx

                                     "D=M")

                              "that"
                              (swap! !out conj
                                     "@THAT"
                                     "D=M" ;; THIS address
                                     (str "@" idx)
                                     "D=D+A"
                                     "A=D" ;; A = THIS + idx

                                     "D=M")

                              "temp"
                              (swap! !out conj
                                     (str "@" (+ TEMP idx))
                                     "D=M")

                              "pointer"
                              (condp = idx
                                0
                                (swap! !out conj
                                       "@THIS"
                                       "D=M")

                                1
                                (swap! !out conj
                                       "@THAT"
                                       "D=M")

                                :else
                                (println "ERROR: pointer index must be 0 or 1."))

                              "static"
                              (swap! !out conj
                                     (str "@" (+ 16 idx))
                                     "D=M")

                              (println "ERROR: No matching push SEGMENT" seg))

                            ;; push onto stack
                            (swap! !out conj

                                     ;; set value at the top of the stack to
                                     ;; the D-register value
                                     "@SP"
                                     "A=M"
                                     "M=D"

                                     ;; increment stack pointer
                                     "@SP"
                                     "M=M+1"))

                          ;; pop  s i; pop top of stack and store into s[i]
                          "pop"
                          (do
                            ;; set Memory address
                            (condp = seg
                              "local"
                              (swap! !out conj
                                     D=SP
                                     (str "@" (+ LCL idx)))

                              "argument"
                              (swap! !out conj
                                     D=SP
                                     (str "@" (+ ARG idx)))

                              "this"
                              (swap! !out conj
                                     "@THIS"
                                     "D=M" ;; THIS address
                                     (str "@" idx)
                                     "D=D+A"
                                     "@R13"
                                     "M=D" ;; R13 = THIS + idx

                                     D=SP

                                     "@R13"
                                     "A=M" ;; A = THIS + idx
                                     )

                              "that"
                              (swap! !out conj
                                     "@THAT"
                                     "D=M"
                                     (str "@" idx)
                                     "D=D+A"
                                     "@R13"
                                     "M=D" ;; R13 = THAT + idx

                                     D=SP

                                     "@R13"
                                     "A=M" ;; A = THAT + idx
                                     )

                              "temp"
                              (swap! !out conj
                                     D=SP
                                     (str "@" (+ TEMP idx)))

                              "pointer"
                              (condp = idx
                                0
                                (swap! !out conj
                                       D=SP
                                       "@THIS")

                                1
                                (swap! !out conj
                                       D=SP
                                       "@THAT")

                                :else
                                (println "ERROR: pointer index must be 0 or 1."))

                              "static"
                              (swap! !out conj
                                     D=SP
                                     (str "@" (+ 16 idx)))

                              (println "ERROR: No matching pop SEGMENT" seg))


                            ;; set value at memory address to D-register value
                            (swap! !out conj
                                   "M=D"))

                          ;; comment
                          "//" nil

                          ;; empty
                          "" nil

                          (println "Line" (inc i)
                                   ". ERROR: No matching operation"
                                   op)))
                      (recur (inc i))))
                  (let [filepath (.getPath file)
                        asm-filename (str (subs filepath 0 (- (count filepath)
                                                              (count ".vm")))
                                          ".asm")]
                    (println "Writing to:" asm-filename)
                    (with-open [w (io/writer asm-filename :append false)]
                      (doseq [s @!out]
                        (.write w (str s "\n"))
                        ;; (println s)
                        ))))

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
