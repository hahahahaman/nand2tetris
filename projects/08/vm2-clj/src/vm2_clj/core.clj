(ns vm2-clj.core
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
     "KBD" 24576})


  ;; init addresses
  (def SP 256)
  (def LCL 300)
  (def ARG 400)
  (def THIS 3000)
  (def THAT 3010)
  (def TEMP 5))

(def init-asm
  (s/join
   "\n"
   [
    ;; init stack pointer: M[0] = 261
    "@261" ;; SP 261, for whatever reason this chapter has SP start at 261
    "D=A"
    "@SP" ;; stack pointer, at address 0. set A=0
    "M=D" ;; M[0]=261

    ;; "@300" ;; LCL 300
    ;; "D=A"
    ;; "@LCL"
    ;; "M=D"

    ;; "@400" ;; ARG 400
    ;; "D=A"
    ;; "@ARG"
    ;; "M=D"

    ;; "@3000" ;; THIS 3000
    ;; "D=A"
    ;; "@THIS"
    ;; "M=D"

    ;; "@3010" ;; THAT 4000
    ;; "D=A"
    ;; "@THAT"
    ;; "M=D"

    ;; call Sys.init
    "@Sys.init"
    "0;JMP"

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

;; D = pop stack
(def D=*SP ;; D register = *SP
  (s/join
   "\n"
   ["@SP"
    "M=M-1" ;; dec stack pointer, SP -= 1

    "A=M"   ;; A = SP-1,
    "D=M"   ;; D = M[SP-1], which is the value at the top of the stack
    ]))

;; push D onto stack
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

;; push 0 onto the stack
(def *SP=0 ;; *SP = 0
  (s/join
   "\n"
   ["@SP"
    "A=M"
    "M=0"

    ;; increment stack pointer
    "@SP"
    "M=M+1"]))

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
      (s/join
       "\n"
       [D=*SP

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
      (s/join
       "\n"
       [D=*SP

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
      (s/join
       "\n"
       [D=*SP ;; pop stack

        dec-SP
        "A=M" ;; A = M[SP-1]
        "D=M-D" ;; D = M[SP-1] - M[SP] = x - y
        (str "@" cond-sym)
        "D;JLT" ;; if (x-y) < 0; jump

        ;; false, x = 0
        "@SP"
        "A=M"
        "M=0"
        (str "@" end-sym)
        "0;JMP"

        ;; true, x = -1
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
        ["@LCL"
         "D=M" ;; D = LCL
         (str "@" idx)
         "A=D+A" ;; A = LCL + idx
         "D=M" ;; D = M[LCL+idx]
         ])

       "argument"
       (s/join
        "\n"
        ["@ARG"
         "D=M"
         (str "@" idx)
         "A=D+A"

         "D=M"])

       "this"
       (s/join
        "\n"
        ["@THIS"
         "D=M" ;; D = THIS
         (str "@" idx)
         "A=D+A" ;; A = THIS + idx

         "D=M"])

       "that"
       (s/join
        "\n"
        ["@THAT"
         "D=M" ;; D = THAT
         (str "@" idx)
         "A=D+A" ;; A = THAT + idx

         "D=M"])

       "temp"
       (s/join
        "\n"
        ["@TEMP"
         "D=M" ;; D = TEMP
         (str "@" idx)
         "A=D+A" ;; A = TEMP + idx

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
       (s/join
        "\n"
        [D=*SP
         "@LCL"
         "D=M"
         (str "@" idx)
         "A=D+A"])

       "argument"
       (s/join
        "\n"
        [D=*SP
         "@ARG"
         "D=M"
         (str "@" idx)
         "A=D+A"])

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
       (s/join
        "\n"
        ["@THAT"
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
       (s/join
        "\n"
        [D=*SP
         "@TEMP"
         "D=M"
         (str "@" idx)
         "A=D+A"])

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
  (let [prefix (if (empty? fn-name) "" (str fn-name "$"))
        label (str prefix seg)]
    (condp = op
     "label"
     (s/join "\n" [(str "(" label ")")])

     "goto"
     (s/join "\n" [(str "@" label)
                   "0;JMP"])

     "if-goto"
     (s/join "\n" [
                   (str "// START if-goto " label)
                   D=*SP ;; pop top of stack
                   (str "@"  label)
                   "D;JNE" ;; jump if stack value != 0

                   (str "// END if-goto " label)
                   ]))))

;; see figure 8.4 of the stack when functions are called
;; see figure 8.5 of the pseudocode generated
;; see figure 8.8 for the transition from function call to after return
;;
;; interesting elegance of function frames. the top of the stack can act as a
;; new, independent stack. fractal self-similarity? reminds me of the unifying
;; interface idea in the witness
(defn gen-function-calling-asm [op seg idx new-label!]
  (condp = op

    "call"
    (let [{return-address-sym :symbol return-address-label :label} (new-label!)]
      (s/join
       "\n"
       [;; call f n - call function f, n arguments have been pushed
        ;; onto the stack

        (str "// call " seg " " idx)
        ;; push return-address
        (str "@" return-address-sym)
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
        "0;JMP"

        ;; (return-address)
        return-address-label]))

    ;; function f k - f is filename, k is # of local variables
    "function"
    (s/join
     "\n"
     (apply conj
            ;; label for function entry
            [(str "(" seg ")")]

            ;; initialize all k local variables to 0
            (repeat idx *SP=0)))

    "return"
    (s/join
     "\n"
     [;; return to the calling function
      "// START return"

      ;; arbitrarily use R14 to store FRAME temp variable
      ;; FRAME = R14 = LCL, memory address of first local variable
      "@LCL"
      "D=M" ;; D = FRAME
      "@R14"
      "M=D" ;; R14 = FRAME

      ;; FRAME-5 is the address that stores the return address
      ;; see figure 8.4
      ;; RET = R15 = *(FRAME-5)
      "@5"
      "D=D-A" ;; D = FRAME-5
      "A=D"   ;; A = FRAME-5
      "D=M"   ;; D = *(FRAME-5) = return address
      "@R15"
      "M=D" ;; R15 = *(FRAME-5)

      ;; the function call has a return value, which is the value at
      ;; the top of the stack, *SP = M[SP]
      ;; *ARG = M[ARG] is the first address of the current frame
      ;; place the return value, *SP, in *ARG
      D=*SP
      "@ARG"
      "A=M" ;; A = ARG
      "M=D" ;; M[ARG] = *SP

      ;; SP = ARG+1
      "D=A+1" ;; D = ARG+1
      "@SP"
      "M=D"   ;; SP = D

      ;; THAT = *(FRAME-1)
      "@R14"
      "D=M"   ;; D = FRAME
      "D=D-1" ;; D = FRAME-1
      "A=D"   ;; A = FRAME-1
      "D=M"   ;; D = *(FRAME-1)
      "@THAT"
      "M=D"   ;; THAT = *(FRAME-1)

      ;; THIS = *(FRAME-2)
      "@R14"
      "D=M"   ;; D = FRAME
      "@2"    ;; A = 2
      "D=D-A" ;; D = FRAME-2
      "A=D"   ;; A = FRAME-2
      "D=M"   ;; D = *(FRAME-2)
      "@THIS"
      "M=D"   ;; THAT = *(FRAME-2)

      ;; ARG = *(FRAME-3)
      "@R14"
      "D=M"   ;; D = FRAME
      "@3"    ;; A = 3
      "D=D-A" ;; D = FRAME-3
      "A=D"   ;; A = FRAME-3
      "D=M"   ;; D = *(FRAME-3)
      "@ARG"
      "M=D"   ;; THAT = *(FRAME-3)

      ;; LCL = *(FRAME-4)
      "@R14"
      "D=M"   ;; D = FRAME
      "@4"    ;; A = 4
      "D=D-A" ;; D = FRAME-4
      "A=D"   ;; A = FRAME-4
      "D=M"   ;; D = *(FRAME-4)
      "@LCL"
      "M=D"   ;; THAT = *(FRAME-4)

      ;; goto RET = R15
      "@R15" ;; A = &R15
      "A=M"  ;; A = R15
      "0;JMP" ;; Jump to M[R15]

      "// END return"
      ])))

(defn translate-file! [file new-label!]
  (let [file? (.isFile file)

        path (.getPath file)
        filename (.getName file)
        extension (get-file-extension (.getName file))]
    (if-not file?
      (println "Skipping" path "not a file.")

      (if-not (= extension ".vm")
        (println "Skipping" path "not a .vm file.")

        (let [filename-without-ext (subs filename 0 (- (count filename)
                                                       (count ".vm")))
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

                          (contains? #{"label" "goto" "if-goto"} op)
                          (gen-program-flow-asm op seg idx @!fn-name)

                          (contains? #{"call" "function" "return"} op)
                          (do
                            (cond (= op "function")
                                  (reset! !fn-name seg)

                                  ;; there can be multiple returns in a function
                                  ;; (= op "return")
                                  ;; (reset! !fn-name "")
                                  )
                            (gen-function-calling-asm op seg idx new-label!))

                          ;; comment or empty
                          (or (= op "") (= op "//" )) nil

                          :else
                          (println "Line" (inc i)
                                   ". ERROR: No matching operation"
                                   op))]

                (when asm (swap! !out conj asm)))

              (recur (inc i))))

          (println "Translated" path)

          @!out)))))

(defn translate-files!
  "Translate files in filepath.

  If the file-or-dir is a .vm file then translate to .asm

  If the file-or-dir is a directory then translate all .vm files into one .asm"
  [^File file-or-dir]
  (let [dir? (.isDirectory file-or-dir)
        files (if dir? (vec (file-seq file-or-dir)) [file-or-dir])

        filename (.getName file-or-dir)
        file-extension (get-file-extension filename)
        path (.getPath file-or-dir)
        out-filepath (if dir?
                       (str path "/" filename ".asm")
                       (when (= file-extension ".vm")
                         (str
                          (subs path 0 (- (count path)
                                          (count ".vm")))
                          ".asm")))

        vm-label "VMLABEL"
        !label-counter (atom 0)
        new-label! (fn []
                     (let [symbol (str vm-label @!label-counter)
                           label (str "(" symbol ")")]
                       (swap! !label-counter inc)
                       {:label label
                        :symbol symbol}))
        all-file-output
        (->> files
             (mapv (fn [file] (translate-file! file new-label!)))
             (filterv some?))
        lines (conj-colls all-file-output)]

    (when (and out-filepath (not-empty lines))
      (write-file! out-filepath lines))))

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
    (let [file-or-dir (io/file (s/trim (nth args 0)))
          exists? (.exists file-or-dir)]
      (if (not exists?)
        (println "ERROR:" file-or-dir " does not exist!")

        (translate-files! file-or-dir)))

    :else
    (usage)))

(defn ch7-stacktest []
  (-main "../../07/StackArithmetic/StackTest/StackTest.vm"))
