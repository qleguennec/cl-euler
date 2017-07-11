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

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
