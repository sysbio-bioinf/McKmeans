;;;;
; File  : Utilities for McKmeans application
; Author: Johann Kraus
;;;;

(ns mckmeans.utils
  (:use clojure.contrib.def)
  (:import (cern.jet.random.sampling RandomSamplingAssistant)
	   (java.io BufferedWriter FileWriter FileOutputStream OutputStreamWriter))
  (:gen-class))

;(import '(cern.jet.random.sampling RandomSamplingAssistant)
;	'(java.io BufferedWriter FileWriter FileOutputStream OutputStreamWriter))
;(use	'(clojure.contrib def))

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
