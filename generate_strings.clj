;; (ns generate-strings)

(defn gen-str [n]
  (dotimes [x n]
    ;; (println
    ;;   ;; (str "HalfAdder(a=in[" x "], b=c" x ", sum=out[" x "], carry=c" (inc x) ");")
    ;;   ;; (str "And(a=a[" x "], b=a[" x "], out=out[" (inc x) "]);")
    ;;   ;; (str "Equals(a=a[" x "], b=b[" x "], out=o" x ");")
    ;;   (str "Mux(a=PrevIn" x ", b=in[" x "], sel=load, out=NextIn" x ");"))
    ;; (println (str "DFF(in=NextIn" x ", out=PrevIn" x ", out=out[" x "]);"))
    ;; (println (str "Register(in=in, load=r" x ", out=RegVal" x ");"));
    (println (str "RAM8(in=in, load=r" x ", address[0]=address[0], address[1]=address[1], out=Ram" x ");"))
    (println "")
    ))
