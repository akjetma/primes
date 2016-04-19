(ns primes.table
  (:require [clojure.string :as string]))

;; ------------------------------------- core table data functionality

(defn table-cells
  [table]
  (apply concat table))

(defn reduce-cells
  "reduces over cell values"
  [f table]
  (reduce
   f
   (table-cells table)))

(defn map-cells
  "returns table with f applied to each cell"
  [f table]
  (map
   (partial map f)
   table))

(defn cell-size
  "returns the length of the largest cell string"
  [table]
  (->> table
       (map-cells (comp count str))
       (reduce-cells max)))

;; ---------------------------- coerce cells into equal length strings

(defn pad-cell
  [left right string]  
  (->> (str string)
       (format (str "%" left "s"))
       (format (str "%-" right "s"))))

(defn pad-cells
  "returns table of equal width left-padded cells"
  [table]
  (let [left-pad (cell-size table)
        right-pad (inc left-pad)
        pad-fn (partial pad-cell left-pad right-pad)]
    (map-cells pad-fn table)))

;; --------------------- add column labels and (optionally) row labels

(defn prepend
  "just want to avoid cons/conj list/vector issues for now"
  [prefix coll]
  (concat [prefix] coll))

(defn attach-labels
  ([table col-labels]
   (prepend col-labels table))
  ([table col-labels row-labels]
   (prepend 
    (prepend "" col-labels) 
    (map prepend row-labels table))))

;; -------------------------------- insert separator elements and rows

(defn underline
  [length]
  (apply str (repeat length "-")))

(defn horizontal-rule
  [cell-size row-length]
  (->> (underline cell-size)
       (repeat row-length)
       (string/join "+")))

(defn attach-separators
  [table]
  (let [hr (horizontal-rule
            (cell-size table)
            (count (first table)))]
    (butlast 
     (mapcat
      (fn [row]
        [(interpose "|" row)
         [hr]])
      table))))

;; ---------------------- output and helper function for general usage

(defn table-str
  [table]
  (string/join
   "\n"
   (map
    string/join
    table)))

(def make-table 
  (comp table-str 
        attach-separators
        pad-cells
        attach-labels))

(def print-table
  (comp println 
        make-table))
