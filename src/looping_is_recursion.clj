(ns looping-is-recursion)

(defn power [base exp]
  (let [iter (fn [acc base exp]
               (if (< exp 1)
                 acc
                 (recur (* acc base) base (dec exp))))]
  (iter 1 base exp)))

(defn last-element [a-seq]
  (let [iter (fn [acc s]
               (if (empty? s)
                 acc
                 (recur (first s) (rest s))))]
    (iter nil a-seq)))

(defn seq= [seq1 seq2]
  (cond
    (empty? seq1) (empty? seq2)
    (empty? seq2) false
    (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [index 0
         s a-seq]
    (cond
      (empty? s) nil
      (pred (first s)) index
      :else (recur (inc index) (rest s)))))

(defn avg [a-seq]
  (loop [items 0
         s a-seq
         total 0]
    (if (empty? s)
      (/ total items)
      (recur (inc items)
             (rest s) 
             (+ total (first s))))))

(defn parity [a-seq]
  (loop [items #{}
         s a-seq]
    (if (empty? s)
      items
      (let [item (first s)
            s' (rest s)]
        (if (contains? items item)
          (recur (disj items item) s')
          (recur (conj items item) s'))))))

(defn fast-fibo [n]
  (loop [f1 0
         f2 1
         n n]
    (if (zero? n)
      f1
      (recur f2 (+ f1 f2) (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [outs []
         s a-seq]
    (if (empty? s)
      outs
      (let [item (first s)]
        (if (not= -1 (.indexOf outs item))
          outs
          (recur (conj outs item) (rest s)))))))
