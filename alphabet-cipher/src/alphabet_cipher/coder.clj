(ns alphabet-cipher.coder)

(def letters (apply vector "abcdefghijklmnopqrstuvwxyz"))

(defn rotate
  "Rotate seq by `n` places."
  [n coll]
  (concat (drop n coll) (take n coll)))

(def chart
  "Substitution chart."
  (mapv #(into [] (rotate % letters)) (range 26)))

(defn lookup
  "Lookup `row` and `col`umn by letter in substitution chart."
  [row col]
  (let [r (.indexOf letters row)
        c (.indexOf letters col)]
    ((chart r) c)))

(defn rev-lookup
  "Reverse lookup of `col`."
  [row col]
  (let [r (.indexOf letters row)]
    (letters (.indexOf (chart r) col))))

(defn encode [keyword message]
  (apply str (map lookup (cycle keyword) message)))

(defn decode [keyword message]
  (apply str (map rev-lookup (cycle keyword) message)))

                                        ; just build up key using cycle and compare
(comment (defn extract-cycle [s]
  (loop [has-repeat false
         key (subs s 0 1)
         remain (subs s 1)]
    (let [repeats (-> key re-pattern (re-seq remain) count)]
      (if (zero? repeats)
        (if has-repeat
          (subs key 0 (dec (count key)))
          (str key remain))
        (recur true (str key (subs remain 0 1)) (subs remain 1))))))
)

(defn extract-cycle [coll]
  (let [n (count coll)
        cycle-eql? (fn [s] (= (take n (cycle s)) (seq coll)))]
    (->> (seq coll)
         (reductions #(cons %2 %1) nil)
         (map reverse)
         (drop-while (comp not cycle-eql?))
         (first))))

(defn decipher [cipher message]
  (apply str (extract-cycle (map rev-lookup message cipher))))

