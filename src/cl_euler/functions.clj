(ns cl-euler.functions
  (:import java.lang.Math
           java.lang.String))

(defn flip
  "Like partial except you supply everything but the first argument."
  ([f b] (fn [a] (f a b)))
  ([f b c] (fn [a] (f a b c)))
  ([f b c d & more]
   (fn [a] (apply f a b c d more))))

(defn sum
  [coll]
  (reduce + 0 coll))

(defn product
  [coll]
  (reduce * 1 coll))

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

(defn div?
  [x a]
  (zero? (mod a x)))

(defn nat-sqrt
  [x]
  (int (Math/sqrt x)))

(defn square?
  [x]
  (let [sqrt (Math/sqrt x)]
    (== (int sqrt) sqrt)))

(defn prime?
  [x]
  (cond
    (= 2 x) true
    (div? x 2) false
    :else (let [sq-x (nat-sqrt x)]
            (loop [a 3]
              (if (= x a)
                true
                (if (div? a x)
                  false
                  (recur (+ 2 a))))))))

(defn primes
  ([] (cons 2 (primes 3)))
  ([n] (if (prime? n)
         (lazy-seq (cons n (primes (+ 2 n))))
         (primes (+ 2 n)))))

(defn single-factors-of
  [x]
  (loop [a 1
         factors []]
    (if (= a x)
      (cons a factors)
      (recur (inc a)
             (if (div? a x)
               (cons a factors)
               factors)))))

(defn single-prime-factors-of
  [x]
  (loop [[p & rest] (take-while-below (inc x) (primes))
         factors []]
    (if (nil? p)
      factors
      (recur rest
             (if (div? p x)
               (cons p factors)
               factors)))))

(defn k-partitions
  [n k]
  (cond
    (= k 0) '()
    (= k 1) (for [a (range 1 n)] [a (- n a)])
    (= k 2) (for [a (range 1 (inc (/ n 2)))] [a (- n a)])
    :else
    (reduce
     (fn [acc x] (into acc
        (map
         (flip conj x)
         (k-partitions (- n x) (dec k)))))
     '()
     (range 1 (/ n 2)))))
