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
    {"SP" 0 ;; Stack pointer: points to the next topmost location in the stack
     "LCL" 1 ;; Points to the base of the current VM function’s local segment
     "ARG" 2 ;; Points to the base of the current VM function’s argument segment
     "THIS" 3 ;; base of the current this segment (within the heap)
     "THAT" 4 ;; base of the current that segment (within the heap)

     ;; RAM[5-12] TEMP 8 registers holds the contents of the temp segment
     "R0" 5
     "R1" 6
     "R2" 7
     "R3" 8
     "R4" 9
     "R5" 10
     "R6" 11
     "R7" 12
     "R8" 13
     "R9" 14
     "R10" 15
     "R11" 16
     "R12" 17

     ;; RAM[13-15] Can be used by the VM implementation as general registers.
     "R13" 18
     "R14" 19
     "R15" 20

     "SCREEN" 16384
     "KBD" 24576}))

;; init addresses
(def SP 256)
(def LCL 300)
(def ARG 400)
(def THIS 3000)
(def THAT 3010)
(def TEMP 5)

(def init-asm
  (s/join
   "\n"
   [
    ;; init stack pointer: M[0] = 256
    "@256" ;; SP 256
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
    "M=D"

    ;; TODO call Sys.init
    ]))

(def inc-SP
  (s/join
   "\n"
   ["@SP"
    "M=M+1"]))

(def dec-SP
  (s/join
   "\n"
   ["@SP"
    "M=M-1"]))

;; pointer notation:
;; SP is the stack pointer symbol
;;
;; in C the address of a variable is &SP == 0
;; in hack assembly to set the A register to &SP would be @SP
;;
;; SP ;; == M[0] ==  M[&SP]
;;
;; to dereference a pointer in hack assembly
;; @SP ;; A = 0
;; A=M ;; A = M[0] = SP
;; M   ;; M[A] = M[M[0]] = *SP
;;
;; dereferencing the pointer, using C * notation:
;; *SP ;; == M[M[0]]
;;
(def D=*SP ;; D register = *SP
  (s/join
   "\n"
   ["@SP"
    "M=M-1"
    "A=M"
    "D=M"]))

(def *SP=D ;;  *SP = D register
  (s/join
   "\n"
   [;; set value at the top of the stack to the in D-register value
    "@SP"
    "A=M"
    "M=D"

    ;; increment stack pointer
    "@SP"
    "M=M+1"]))

(def *SP=0 ;; *SP = 0
  (s/join
   "\n"
   ["@SP"
    "A=M"
    "M=0"

    ;; increment stack pointer
    "@SP"
    "M=M+1"]))

(def *SP+=D
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
             D=*SP

             ;; "@SP" ;; A = 0
             ;; "M=M-1" ;; M[0] = M[0]-1  ;; M[0] = 256
             dec-SP

             "A=M" ;; A = M[0]            ;; A = 256
             "M=M+D" ;; M[256]=M[256]+M[257]

             ;; "@SP"
             ;; "M=M+1" ;; M[0] = 257
             inc-SP])

    "sub"
    (s/join "\n" [D=*SP

                  dec-SP

                  "A=M"
                  "M=M-D"

                  inc-SP])

    "neg"
    (s/join "\n" [dec-SP

                  "A=M"
                  "M=-M"

                  inc-SP])

    "and"
    (s/join "\n"
            [D=*SP

             dec-SP

             "A=M"
             "M=M&D"


             inc-SP])

    "or"
    (s/join "\n" [D=*SP

                  dec-SP

                  "A=M"
                  "M=M|D"

                  inc-SP])

    "not"
    (s/join "\n" [dec-SP

                  "A=M"
                  "M=!M"

                  inc-SP])))

(defn gen-comparison-asm! [op new-label!]
  (condp = op
    "eq"
    ;; -1 is true, 0 is false
    (let [{cond-sym :symbol cond-label :label} (new-label!)
          {end-sym :symbol end-label :label} (new-label!)]
      (s/join "\n" [D=*SP

                    dec-SP

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
                    inc-SP]))

    "gt"
    (let [{cond-sym :symbol cond-label :label} (new-label!)
          {end-sym :symbol end-label :label} (new-label!)]
      (s/join "\n" [D=*SP

                    dec-SP

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
                    inc-SP]))

    "lt"
    (let [{cond-sym :symbol cond-label :label} (new-label!)
          {end-sym :symbol end-label :label} (new-label!)]
      (s/join "\n" [D=*SP

                    dec-SP

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
                    inc-SP]))))

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
     *SP=D)

    ;; pop  s i; pop top of stack and store into s[i]
    "pop"
    (str

     ;; first get the address of s[i]
     ;; D=*SP pops top of stack and places the value in D-register
     (condp = seg
       "local"
       (s/join "\n" [D=*SP
                     (str "@" (+ LCL idx))])

       "argument"
       (s/join "\n" [D=*SP
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

         D=*SP

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

                     D=*SP

                     "@R13"
                     "A=M" ;; A = THAT + idx
                     ])

       "temp"
       (s/join "\n" [D=*SP
                     (str "@" (+ TEMP idx))])

       "pointer"
       (condp = idx
         0
         (s/join "\n" [D=*SP
                       "@THIS"])

         1
         (s/join "\n" [D=*SP
                       "@THAT"])

         :else
         (println "ERROR: pointer index must be 0 or 1."))

       "static"
       (s/join "\n" [D=*SP
                     (str "@" filename "." idx)])

       (println "ERROR: No matching pop SEGMENT" seg))

     "\n"

     ;; second set value at memory address to D-register value
     "M=D"
     )))

;; labels inside a function needs to prepend functionName$
;; functionName$label
;; see figure 8.6
(defn gen-program-flow-asm [op seg idx fn-name]
  (condp = op
    "label"
    (s/join "\n" [(str "(" fn-name "$" seg ")")])

    "goto"
    (s/join "\n" [(str "@" fn-name "$" seg)
                  "0;JMP"])

    "if-goto"
    (s/join "\n" [D=*SP ;; pop top of stack
                  (str "@" fn-name "$" seg)
                  "D;JNE" ;; jump if stack value != 0
                  ])))


;; TODO
;; see figure 8.4 of the stack when functions are called
;; see figure 8.5 of the pseudocode generated
;;
;; interesting elegance of function frames. the top of the stack can act as a
;; new, independent stack. fractal self-similarity? reminds me of the unifying
;; interface idea in the witness
(defn gen-function-calling-asm [op seg idx new-label!]
  (condp = op

    "call"
    (let [return-address (new-label!)]
      (s/join "\n" [;; call f n - call function f, n arguments have been pushed
                    ;; onto the stack

                    ;; push return-address
                    (str "@" return-address)
                    "D=A"
                    *SP=D

                    ;; push LCL
                    "@LCL"
                    "D=M"
                    *SP=D

                    ;; push ARG
                    "@ARG"
                    "D=M"
                    *SP=D

                    ;; push THIS
                    "@THIS"
                    "D=M"
                    *SP=D

                    ;; push THAT
                    "@THAT"
                    "D=M"
                    *SP=D

                    ;; ARG = SP-5-n
                    ;; we just pushed 5 values to the stack
                    ;; go back 5
                    ;; we have n args, so go back n
                    "@SP"
                    "D=M"
                    (str "@" (+ 5 idx))
                    "D=D-A" ;; SP - (5 + n)
                    "@ARG"
                    "M=D"

                    ;; LCL = SP
                    "@SP"
                    "D=M"
                    "@LCL"
                    "M=D"

                    ;; goto f
                    (str "@" seg)

                    ;; (return-address)
                    (str "(" return-address ")")]))

    ;; function f n - f is filename, n is # of local variables
    "function"
    (s/join "\n"
            (apply conj
                   ;; label for function entry
                   [(str "(" seg ")")]

                   ;; initialize all n args to 0
                   (repeat idx *SP=0)))


    "return"
    (s/join "\n" [;; return to the calling function

                  ;; FRAME = R14 = LCL, memory address of first local variable
                  "@LCL"
                  "D=M" ;; D = FRAME
                  "@R14"
                  "M=D" ;; R14 = FRAME

                  ;; RET = R15 = *FRAME -5, return address
                  "A=M" ;; A = FRAME
                  "D=M" ;; D = *FRAME
                  "@5"
                  "D=D-A" ;; D = *FRAME -5
                  "@R15"
                  "M=D" ;; R15 = *FRAME -5

                  ;; *ARG = pop()
                  D=*SP ;; pop()
                  "@ARG"
                  ""

                  ;; SP = ARG+1
                  ;; THAT = *FRAME -1
                  "@R14"
                  "D=M"

                  ;; THIS = *(FRAME-2)
                  ;; ARG = *(FRAME-3)
                  ;; LCL = *(FRAME-4)

                  ;; goto RET = R15
                  "@R15" ;; A = &R15
                  "A=M"  ;; A = M[&R15] = R15
                  "0;JMP" ;; Jump to M[R15]
                  ])
    ))

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

        !out (atom [init-asm])
        !fn-name (atom "")
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

                    (contains? #{"call" "function" "return"} op)
                    (do
                      (cond (= op "function")
                            (reset! !fn-name seg)

                            (= op "return")
                            (reset! !fn-name ""))
                      (gen-function-calling-asm op seg idx new-label!))

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
