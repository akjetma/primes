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

;; --------------------- implementation of bounded erastosthenes sieve

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
  "Description of parameters:
  - state: map of values that are updated only when a prime is found--grouped
    together to prevent re-branching.
      - search-space: a vector of booleans describing whether the number
        at each index is potentially a prime. The sieve iterates through this
        list, adding primes to `primes` and flipping the flags of the prime's
        multiples in `search-space` when it encounters a `true` value.
      - primes: list of found primes
      - prime-count: self-explanatory. re-running the count function for
        every number in the search space can be costly later on
  - prime-limit: the number of primes requested. prevents us from searching
    extraneous values after we have found enough primes
  - search-limit: the size of search-space vector. repeatedly recounting 
    was expensive
  - search: the number being checked for primality."
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
  "returns a list of `num-primes` primes"
  [num-primes]
  (when (pos? num-primes)
    (let [search-size (upper-bound num-primes)]
      (sieve {:search-space (vec (repeat search-size true)) 
              :primes []
              :prime-count 0}
             num-primes
             search-size
             2))))
