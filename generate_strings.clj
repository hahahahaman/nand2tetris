;; (ns generate-strings)

(defn gen-str [n]
  (dotimes [x n]
    (println
      (str "HalfAdder(a=in[" x "], b=c" x ", sum=out[" x "], carry=c" (inc x) ");"))))
