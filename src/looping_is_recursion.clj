(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
  								(if (zero? exp)
  									acc
  									(recur (* acc base) base (dec exp))))]
  	(helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
  	(first a-seq)
  	(recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (if (and (empty? seq1) (empty? seq2))
  	true
  	(if (or (not (= (first seq1) (first seq2))) (empty? seq1) (empty? seq2))
  		false
  		(recur (rest seq1) (rest seq2)))))

(defn find-first-index [pred a-seq]
  (loop [pred1 pred
  			 seq1 a-seq
  			 i 0]
  	(cond
  		(empty? seq1) nil
  		(pred1 (first seq1)) i
  		:else (recur pred1 (rest seq1) (inc i)))))

(defn avg [a-seq]
  (loop [seq1 a-seq
  			 sum 0
  			 i 0]
  	(cond
  		(empty? seq1) (/ sum i)
  		:else (recur (rest seq1) (+ sum (first seq1)) (inc i)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [set1 #{}
  			 seq1 a-seq]
  	(if (empty? seq1)
  		set1
  		(recur (toggle set1 (first seq1)) (rest seq1)))))

(defn fast-fibo [n]
  (loop [my-n 1
         n-1 0
         i 1
         fib n]
    (cond
      (<= fib 0) 0
      (= fib 1) 1
      (= i fib) my-n
      :else (recur (+ my-n n-1) my-n (inc i) fib))))

(defn cut-at-repetition [a-seq]
  [":("])

