(ns primes.primes
  (:require [clojure.set :as set]))

;; -------------------------------------------- testing util functions

(comment
  (defonce hit-count (atom 0))

  (defn hit
    []
    (swap! hit-count inc))

  (defn check
    [algo n]
    (reset! hit-count 0)
    (let [v (time (algo n))]
      (println "hit counter: " @hit-count)
      (println "prime count: " (count v))
      (println "last prime: " (last v)))))

;; ---------------- slow implementation of bounded erastosthenes sieve

(defn upper-bound
  "rough upper bound for the nth prime. 

  for example, 
  - the millionth prime is 15,485,863
  - this function gives 16,441,302 for the upper bound
  - there are 955,439 numbers between the prime and the bound
  - there are 57,661 primes in that space
  - the benefit of using the bounded sieve rather than the naiive 
    implementation outweighs the penalty of the larger search space.
  
  does not work for primes 2-11 (first 5 primes).
  
  using the bounded sieve vs. naiive implementation 

  source: https://www.maa.org/sites/default/files/jaroma03200545640.pdf"
  [num-primes]
  (if (< num-primes 6) 
    12
    (int 
     (+ (* num-primes (Math/log num-primes)) 
        (* num-primes (Math/log (Math/log num-primes)))))))

(defn set-composites
  "flag the multiples of a number in the search-space.
  false means 'not a prime'"
  [search-space search-limit search]
  (reduce
   (fn [numbers composite]
     (assoc numbers composite false))
   search-space
   (range search search-limit search)))

(defn sieve  
  [{:as state :keys [search-space primes prime-count]}
   prime-limit search-limit search]   
  (if (or (= prime-count prime-limit) 
          (= search search-limit)) 
    primes
    (let [next-state 
          (if (get search-space search)
            {:search-space (set-composites search-space search-limit search)
             :primes (conj primes search)
             :prime-count (inc prime-count)}
            state)]
      (recur next-state prime-limit search-limit (inc search)))))

(defn generate
  "sets up data for sieve function"
  [num-primes]
  (when (pos? num-primes)
    (let [search-size (upper-bound num-primes)]
      (sieve {:search-space (vec (repeat search-size true))
              :primes []
              :prime-count 0}
             num-primes        ;; prime-limit
             search-size       ;; search-limit
             2))))              ;; search (number being checked)
