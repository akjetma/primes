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

;; ----------------------------------------- upper bound for nth prime

(defn upper-bound
  [num-primes]
  (if (< num-primes 6) 
    12
    (int 
     (+ (* num-primes (Math/log num-primes)) 
        (* num-primes (Math/log (Math/log num-primes)))))))

;; --------------------------------- performed when sieve hits a prime

(defn multiples
  [of to]
  (let [from (* 2 of)]
    (range from to of)))

(defn set-composites
  [search-space composites]
  (reduce
   #(assoc %1 %2 false)
   search-space
   composites))

(defn new-prime
  [{:keys [search search-limit] :as state}]
  (-> state
      (update :search-space set-composites (multiples search search-limit))
      (update :primes conj search)
      (update :prime-count inc)
      (update :search inc)))

;; -------------- performed for every composite until all primes found

(defn no-prime
  [state]
  (update state :search inc))

(defn prime?
  [{:keys [search-space search]}]
  (get search-space search))

(defn stop?
  [{:keys [prime-count prime-limit search-limit search]}]
  (or (= prime-count prime-limit) 
      (= search search-limit)))

;; ----------------------------------------------------------- process

(defn sieve
  [state]
  (if (stop? state)
    (:primes state)
    (recur (if (prime? state) 
             (new-prime state) 
             (no-prime state)))))

(defn generate
  [num-primes]
  (when (pos? num-primes)
    (let [search-size (upper-bound num-primes)]
      (sieve {:search-space (vec (repeat search-size true)) 
              :primes []
              :prime-count 0
              :prime-limit num-primes
              :search-limit search-size
              :search 2}))))
