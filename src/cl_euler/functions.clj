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
