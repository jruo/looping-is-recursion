(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (= exp 0)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (<= (count a-seq) 1)
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond (and (empty? seq1) (empty? seq2)) true
        (or (empty? seq1) (empty? seq2)) false
        :else
          (let [first1 (first seq1)
                first2 (first seq2)]
            (if (not (= first1 first2))
              false
              (recur (rest seq1) (rest seq2))))))

(defn find-first-index [pred a-seq]
  (loop [index 0]
    (if (= index (count a-seq))
      nil
      (if (pred (nth a-seq index))
        index
        (recur (inc index))))))

(defn avg [a-seq]
  (loop [index 0
         sum 0]
    (if (= index (count a-seq))
      (/ sum index)
      (recur (inc index) (+ sum (nth a-seq index))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [index 0
         a-set #{}]
    (if (= index (count a-seq))
      a-set
      (recur (inc index) (toggle a-set (nth a-seq index))))))

(defn fast-fibo [n]
  (loop [a 0
         b 1
         i 0]
    (if (= i n)
      a
      (recur b (+ a b) (inc i)))))

(defn cut-at-repetition [a-seq]
  (loop [prevs #{}
         return []
         index 0]
    (if (= index (count a-seq))
      return
      (let [cur (nth a-seq index)]
        (if (contains? prevs cur)
          return
          (recur (conj prevs cur) (conj return cur) (inc index)))))))
