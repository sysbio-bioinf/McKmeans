;;;;
; File  : Utilities for McKmeans application
; Author: Johann Kraus
;;;;

(ns mckmeans.utils
  (:use clojure.contrib.def clojure.contrib.duck-streams)
  (:import (cern.jet.random.sampling RandomSamplingAssistant)
	   (java.io BufferedWriter FileWriter FileOutputStream OutputStreamWriter))
  (:gen-class))

;(import '(cern.jet.random.sampling RandomSamplingAssistant)
;	'(java.io BufferedWriter FileWriter FileOutputStream OutputStreamWriter))
;(use	'(clojure.contrib def duck-streams))

(defn split-string
  [#^String s #^String sc]
  (. s (split sc)))

(defn string-to-double
  [#^String s]
  (. Double (parseDouble s)))

(defn string-to-int
  [#^String s]
  (Integer/parseInt s))

(defn parse-tab-line
  [#^String s]
  (double-array (map string-to-double (split-string s "\t"))))

(defn parse-tab-line-snp
  [#^String s]
  (int-array (map string-to-int (split-string s "\t"))))

(defn load-tab-file
  [filename snp]
  (if-not snp
    (vec (doall (pmap parse-tab-line (split-string (slurp filename) "\n"))))
    (vec (doall (pmap parse-tab-line-snp (split-string (slurp filename) "\n"))))))

(defn csv-parse-string
  "Get string-arry from csv-like string using separator sc, e.g. 'a,b,3,c.d'."
  [#^String s #^String sc]
  (.split s sc))

(defn csv-parse-int
  "Get int-arry from csv-like string using separator sc, e.g. 'a,b,3,c.d'."
  [#^String s #^String sc]
  (let [string-array (.split s sc)]
    (int-array (map #(Integer/parseInt %) string-array))))

(defn csv-parse-double
  "Get double-arry from csv-like string using separator sc, e.g. '1,2,3,4.5'."
  [#^String s #^String sc]
  (let [string-array (.split s sc)]
    (double-array (map #(Double/parseDouble %) string-array))))

(defn read-csv
  [filename sc header csvparser]
  (let [lines (read-lines filename)]
    (vec (doall (map #(apply csvparser % (list sc)) (drop (if header 1 0) lines))))))

(defn save-result
  [#^String filename #^String res]
  (with-open [out (BufferedWriter. (FileWriter. filename))]
    (.write out res)))

(defn save-append-result
  [#^String filename #^String res]
  (with-open [out (BufferedWriter. (FileWriter. filename true))]
    (.write out res)))

(defnk save-txt
  [#^String filename #^String res :append false]
  (with-open [out (BufferedWriter. (FileWriter. filename :append))]
    (.write out res)))

(defn sample
  "Randomly take k elements from sequence of length n without replacement"
  [n k]
  (seq (. RandomSamplingAssistant (sampleArray k (int-array (range n))))))

(defn whichmin
  [#^doubles xs]     
  (areduce xs i ret 0          
	   (if (< (aget xs i) (aget xs ret)) 
	     i
	     ret)))

(defn whichmax
  [#^doubles xs]
  (areduce xs i ret 0          
	   (if (> (aget xs i) (aget xs ret))
	     i
	     ret)))

(defn distance
  [#^doubles as #^doubles bs]
  (areduce as i ret (double 0)
	   (+ ret (* (- (aget as i) (aget bs i)) (- (aget as i) (aget bs i))))))

(defn distance-snp
  [#^ints as #^ints bs]
  (areduce as i ret (int 0)
	   (+ ret (Math/abs (Integer/signum (unchecked-subtract (aget as i) (aget bs i)))))))

(defn da+ 
  [#^doubles as #^doubles bs]
  (amap as i ret
	(+ (aget as i) (aget bs i))))

(defn da-snp-freq
  [#^ints as #^ints bs]
  (let [mod 3]
    (areduce bs i ret (int 0)
	     (* ret (int (aset-int as (+ (* i mod) (aget bs i)) (inc (aget as (+ (* i mod) (aget bs i)))))))))
  as)

(defn da-snp-mode
  [#^ints as]
  (let [bs (int-array (/ (alength as) 3))
	mod (int 3)
	pos1 (int 1)
	pos2 (int 2)]
    (areduce bs i ret (int 0)
	     (* ret (int (aset-int bs i (whichmax (double-array 
			(list (aget as (* i mod)) (aget as (+ (* i mod) pos1)) (aget as (+ (* i mod) pos2)))))))))
    bs))

(defn split-seq
  [len k]
  (let [s (. Math (floor (/ len k)))]
    (loop [res (list s)]
      (if (< (count res) k)
	(recur (conj res (+ (first res) s)))
	(reverse (conj (drop 1 res) len))))))

(defn pairwise-comb [n]
  (let [n (dec (int n))]
    (loop [start (int 0) counter (int 1) res nil]
      (if (= start n)
	res
	(if (= counter n)
	  (recur (inc start) (inc (inc start)) (conj res (list start counter)))
	  (recur start (inc counter) (conj res (list start counter))))))))

(defn transpose [dat snpmode]
  (let [nrow (int (count dat))
	ncol (int (alength (first dat)))
	ilist (range ncol)]
    (loop [idx (dec nrow)
	   tran (vec (replicate ncol '()))]
      (if (>= idx 0)
	(let [curdat (dat idx)]
	  (recur (dec idx) (vec (doall (map #(cons (aget curdat %) (tran %)) ilist)))))
	(if-not snpmode
	  (vec (doall (map #(double-array %) tran)))
	  (vec (doall (map #(int-array %) tran))))))))

