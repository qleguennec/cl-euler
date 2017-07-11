(ns cl-euler.functions)

(defn sum
  [coll]
  (reduce + 0 coll))

(defn take-while-below
  [n coll]
  (take-while #(< % n) coll))

(defn multiples-of
  [x]
  (iterate (partial + x) x))

(defn fibs
  []
  (iterate
   (fn [[fst snd :as all]]
     (conj all (+ fst snd))) '(2 1)))

(defn fib
  [n]
  (first (nth (fibs) n)))
