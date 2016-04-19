(ns primes.core
  (:gen-class)
  (:require [primes.primes :as primes]
            [primes.table :as table]))

(defn multiplication-table
  [numbers]
  (map
   (fn [x]
     (map
      (fn [y]
        (* x y))
      numbers))
   numbers))

(defn print-table
  [n]
  (let [n-primes (primes/generate n)
        products (multiplication-table n-primes)]
    (table/print-table products n-primes n-primes)))

(defn -main
  ([] (-main "10"))
  ([n] (print-table (Integer/parseInt n))))
