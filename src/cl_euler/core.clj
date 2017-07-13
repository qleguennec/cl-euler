(ns cl-euler.core
  (:use [cl-euler.functions]))

(defn pb-01
  [n]
  (let [m3 (vec (take-while-below n (multiples-of 3)))
        m5 (vec (take-while-below n (multiples-of 5)))]
    (sum (reduce (fn [acc x]
                   (if (zero? (mod x 15))
                     acc
                     (conj acc x)))
                  (take-while-below n (multiples-of 15))
                  (into m3 m5)))))

(defn pb-02
  [n]
  (reduce (fn [acc x]
            (if (even? x)
              (+ acc x)
              acc))
          0
          (last (take-while #(< (first %) n) (fibs)))))

(defn pb-03
  [x]
  (loop [a 2
         rest x]
    (cond
      (= a rest) a
      (not (div? a rest)) (recur (inc a) rest)
      :else (recur 3 (/ rest a)))))

(defn pb-05
  [n]
  (loop [[d & divs] (range 2 (inc n))
         acc 1]
    (if (nil? d)
      acc
      (recur
       (map #(let [x (/ % d)]
               (if (integer? x) x %))
            divs)
       (* acc d)))))

(defn pb-06
  [n]
  (- (let [s (sum (range 1 (inc n)))] (* s s))
     (reduce (fn [acc x] (+ acc (* x x))) 0 (range 1 (inc n)))))

(defn pb-07
  [n]
  (nth (primes) (dec n)))

(defn pb-08
  [n]
  (loop [[x & rest :as digits]
         (map #(- (int %) (int \0)) (slurp "./resources/pb-08.txt"))
         max-prod 0]
    (if (nil? x) max-prod
        (let [prod (product (take n digits))]
          (recur rest (if (> prod max-prod) prod max-prod))))))

(defn pb-09
  [n]
  (loop [[[a b c] & parts] (map sort (k-partitions n 3))]
        (if (= (* c c) (+ (* a a) (* b b)))
          (* a b c)
          (recur parts))))

(defn pb-10
  [n]
  (sum (take-while-below n (primes))))
