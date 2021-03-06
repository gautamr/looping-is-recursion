(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc k]
                 (if (zero? k)
                   acc
                   (recur (* base acc) (dec k))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [coll]
                 (if (or (empty? coll) (= (count coll) 1))
                   (first coll)
                   (recur (rest coll))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq-1 seq-2]
                 (let [f1 (first seq-1) f2 (first seq-2)]
                   (cond
                     (empty? seq-1) (empty? seq-2)
                     (empty? seq-2) (empty? seq-1)
                     (not= f1 f2) false
                     :else (recur (rest seq-1) (rest seq-2)))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [idx 0 coll a-seq]
    (cond
      (empty? coll) nil
      (pred (first coll)) idx
      :else (recur (inc idx) (rest coll)))))

(defn avg [a-seq]
  (loop [acc 0 coll a-seq]
    (if (empty? coll)
      (/ acc (count a-seq))
      (recur (+ acc (first coll)) (rest coll)))))

(defn parity [a-seq]
  (loop [acc-set #{} coll a-seq]
    (let [f (first coll)]
      (if (empty? coll)
        (reverse acc-set)
        (recur (if (contains? acc-set f) (disj acc-set f) (conj acc-set f)) (rest coll))))))

(defn fast-fibo [n]
  (loop [f0 0 f1 1 nth 0]
    (if (= n nth)
      f0
      (recur f1 (+ f0 f1) (inc nth)))))

;; contains? is tricky to use in vector so two collections are used
(defn cut-at-repetition [a-seq]
  (loop [acc [] seen #{} coll a-seq]
    (let [f (first coll)]
      (if (or (contains? seen f) (empty? coll))
        acc
        (recur (conj acc f) (conj seen f) (rest coll))))))
