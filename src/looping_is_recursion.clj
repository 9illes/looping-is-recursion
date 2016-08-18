(ns looping-is-recursion)

(defn power [base exp]
  (let [acc base
        helper (fn [acc exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [el r-seq]
                 (if (empty? r-seq)
                   el
                   (recur (first r-seq) (rest r-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [eq s1 s2]
                 (cond
                   (not (= (count seq1) (count seq2))) false
                   (and (empty? s1) (empty? s2)) eq
                   :else (recur (and eq (= (first s1) (first s2))) (rest s1) (rest s2))))]
    (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [my-seq a-seq
         pos 0]
    (cond
      (empty? my-seq) nil
      (pred (first my-seq)) pos
      :else (recur (rest my-seq) (+ 1 pos)))))

(defn avg [a-seq]
  (loop [sum 0
         my-seq a-seq]
    (if (empty? my-seq)
      (/ sum (count a-seq))
      (recur (+ sum (first my-seq)) (rest my-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn parity [a-seq]
  (loop [a-seq a-seq
         a-set #{}]
    (cond
      (empty? a-seq) a-set
      :else (recur (rest a-seq) (toggle a-set (first a-seq))))))

(defn fast-fibo [n]
   (loop [f--1 0
          f-n 1
          n n]
     (cond
       (= 0 n) f--1
       (= 1 n) f-n
       :else (recur f-n (+ f--1 f-n) (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [a-seq a-seq
         r-seq []]
    (cond
      (empty? a-seq) r-seq
      (some #{(first a-seq)} r-seq) r-seq
      :else (recur (rest a-seq) (conj r-seq (first a-seq))))))
