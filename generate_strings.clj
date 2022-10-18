;; (ns generate-strings)

(defn gen-str [n]
  (dotimes [x n]
    (println
      ;; (str "HalfAdder(a=in[" x "], b=c" x ", sum=out[" x "], carry=c" (inc x) ");")
      ;; (str "And(a=a[" x "], b=a[" x "], out=out[" (inc x) "]);")
      ;; (str "Equals(a=a[" x "], b=b[" x "], out=o" x ");")
      (str "And(a=andout" x ", b=o" (+ x 2) ", out=andout" (inc x) ");")
      )))
