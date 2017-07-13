(ns cl-euler.core
  (:gen-class)
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

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
