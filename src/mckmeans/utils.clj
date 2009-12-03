;;;;
; File  : Utilities for McKmeans application
; Author: Johann Kraus
;;;;

(ns mckmeans.utils
  (:use clojure.contrib.def clojure.contrib.duck-streams clojure.contrib.seq-utils)
  (:import (cern.jet.random.sampling RandomSamplingAssistant)
	   (java.io BufferedWriter FileWriter FileOutputStream OutputStreamWriter PrintWriter);)

	   (javax.swing JFrame JLabel JButton JPanel JMenuBar JMenu JMenuItem JFileChooser JTextField JCheckBox JTextArea JScrollPane JTabbedPane JOptionPane SwingUtilities GroupLayout JEditorPane JList DefaultListModel)
	   (javax.swing.filechooser FileFilter)
	   (javax.swing.event HyperlinkListener)
	   (java.awt.event ActionListener KeyListener)
	   (java.awt FlowLayout GridLayout BorderLayout Color)
	   (java.io BufferedWriter FileWriter FileOutputStream OutputStreamWriter)
	   (org.jfree.data.xy DefaultXYDataset)
	   (org.jfree.chart.axis CategoryAnchor AxisLocation NumberAxis)
	   (org.jfree.data.statistics DefaultBoxAndWhiskerCategoryDataset)
	   (org.jfree.data.category DefaultCategoryDataset)
	   (org.jfree.chart ChartFactory JFreeChart ChartPanel)
	   (org.jfree.chart.plot PlotOrientation CombinedDomainXYPlot)
	   (org.apache.batik.dom GenericDOMImplementation)
	   (org.apache.batik.svggen SVGGraphics2D)
	   (org.w3c.dom DOMImplementation Document))

  (:gen-class))

;(import '(cern.jet.random.sampling RandomSamplingAssistant)
;	'(java.io BufferedWriter FileWriter FileOutputStream OutputStreamWriter))
;(use	'(clojure.contrib def duck-streams))

(defn median
  [x]
  (let [n (int (count x))
	y (sort x)]
    (if (odd? n)
      (nth y (/ n 2))
      (/ (apply + (take 2 (drop (/ (- n 2) 2) y))) 2))))

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
  (let [lines (read-lines filename)
	sc (list sc)]
;    (vec (doall (map #(apply csvparser % sc) (drop (if header 1 0) lines))))))
    (vec (doall (map #(apply csvparser % sc) (drop (if (. (first (first lines)) equals (first "#")) 1 0) lines))))))

(defn list-parse-csv
  [l #^String sc]
  (.. (str l) (replace "(" "") (replace ")" "") (replace " " sc)))

(defn vec-parse-csv
  [l #^String sc]
  (.. (str l) (replace "[" "") (replace "]" "") (replace " " sc)))

(defn write-csv
  [lines #^String filename sc header csvparser]
  (let [sc (list sc)]
    (with-open [writer (PrintWriter. (BufferedWriter. (FileWriter. filename)))]
      (dorun (map #(.println writer (apply csvparser % sc)) lines)))))

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
  (with-open [out (BufferedWriter. (FileWriter. filename (boolean :append)))]
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
  "Simple matching distance for SNP data"
  [#^ints as #^ints bs]
  (areduce as i ret (int 0)
	   (+ ret (Math/abs (Integer/signum (unchecked-subtract (aget as i) (aget bs i)))))))

(defn distance-asd
  "Allele sharing distance for SNP data"
  [#^ints as #^ints bs]
  (/ (areduce as i ret (int 0)
	   (+ ret (Math/abs (unchecked-subtract (aget as i) (aget bs i)))))
     (alength as)))

(defn da+ 
  [#^doubles as #^doubles bs]
  (amap as i ret
	(+ (aget as i) (aget bs i))))

(defn da-snp-freq
  [#^ints as #^ints bs]
  (let [mod (int 3)]
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

;; (defn pairwise-comb [n]
;;   (let [n (dec (int n))]
;;     (loop [start (int 0) counter (int 1) res nil]
;;       (if (= start n)
;; 	res
;; 	(if (= counter n)
;; 	  (recur (inc start) (inc (inc start)) (conj res (list start counter)))
;; 	  (recur start (inc counter) (conj res (list start counter))))))))

(defn pairwise-comb [n]
  (let [n (dec (int n))]
    (loop [start (int 0) counter (int 1) res []]
      (if (= start n)
	res
	(if (= counter n)
	  (recur (inc start) (inc (inc start)) (conj res [start counter]))
	  (recur start (inc counter) (conj res [start counter])))))))

(defn transpose [dat snpmode]
  (let [nrow (int (count dat))
	ncol (int (alength (first dat)))
	ilist (range ncol)]
    (loop [idx (dec nrow)
	   tran (vec (replicate ncol '()))]
      (if (>= idx 0)
	(let [curdat (dat idx)]
	  (recur (dec idx) (vec (doall (map #(cons (aget curdat (int %)) (tran (int %))) ilist)))))
	(if-not snpmode
	  (vec (doall (map #(double-array %) tran)))
	  (vec (doall (map #(int-array %) tran))))))))

(defmacro init-array [type init-fn & dims]
  (let [idxs (map (fn [_] (gensym)) dims)
        ds (map (fn [_] (gensym)) dims)]
    `(let [a# (make-array ~type ~@dims)
           f# ~init-fn
           ~@(mapcat vector ds dims)]
       (dorun 
        (for ~(vec (mapcat #(list %1 (list `range %2)) idxs ds))
          (aset a# ~@idxs (f# ~@idxs))))
       a#)))

(defmacro deep-aget
  ([hint array idx]
    `(aget ~(vary-meta array assoc :tag hint) ~idx))
  ([hint array idx & idxs]
    `(let [a# (aget ~(vary-meta array assoc :tag 'objects) ~idx)]
       (deep-aget ~hint a# ~@idxs))))

(defmacro deep-aset [hint array & idxsv]
  (let [hints '{doubles double ints int} ; writing a comprehensive map is left as an exercise to the reader
        [v idx & sxdi] (reverse idxsv)
        idxs (reverse sxdi)
        v (if-let [h (hints hint)] (list h v) v)
        nested-array (if (seq idxs)
                       `(deep-aget ~'objects ~array ~@idxs)
                        array)
        a-sym (with-meta (gensym "a") {:tag hint})]
      `(let [~a-sym ~nested-array]
         (aset ~a-sym ~idx ~v))))

(defn sammon [dat maxiter dim]
  (let [maxiter (int maxiter)
	nrow (int (count dat))
	proj (init-array Double/TYPE (fn [_ _] (rand) (rand)) nrow dim)] ; this hurts immutability!
    (dotimes [iter maxiter]
      (let [lambda (/ 1 (inc iter))]
	(dotimes [i nrow]
	    (dotimes [j nrow]
	      (if (> j i)
		(let [#^doubles dat0 (dat i)
		      #^doubles proj0 (aget proj i)
		      #^doubles proj1 (aget proj j)
		      di (Math/sqrt (double (distance proj0 proj1)))
		      #^doubles dat1 (dat j)
		      dist (Math/sqrt (double (distance dat0 dat1)))
		      delta (double (/ (* lambda (- dist di)) di))
		      #^doubles newproj0 (amap proj0 idx ret
					       (+ (aget proj0 idx) (* delta (- (aget proj0 idx) (aget proj1 idx)))))
		      #^doubles newproj1 (amap proj1 idx ret
					       (- (aget proj1 idx) (* delta (- (aget proj0 idx) (aget proj1 idx)))))]
		  (aset proj i newproj0)
		  (aset proj j newproj1)))))))
    (vec proj)))

;(defn sammon [dat maxiter dim]
;  (let [nrow (count dat)
;	combs (reverse (pairwise-comb nrow))
;	dists (vec (doall (map (fn [x] (distance (nth dat (first x)) (nth dat (second x)))) combs)))
;	proj (init-array Double/TYPE (fn [_ _] (rand) (rand)) nrow dim)] ; this hurts immutability!
;    (loop [iter (int 1)
;	   lambda 1]
;      (if (< iter maxiter)
;	(do (dorun (map (fn [x y] (let [proj0 (aget proj (first x))
;					proj1 (aget proj (second x))
;					di (distance proj0 proj1)
;					delta (/ (* lambda (- y di)) di)
;					newproj0 (amap proj0 idx ret
;					   (+ (aget proj0 idx) (* delta (- (aget proj0 idx) (aget proj1 idx)))))
;					newproj1 (amap proj0 idx ret
;					   (- (aget proj1 idx) (* delta (- (aget proj0 idx) (aget proj1 idx)))))]
;				    (aset proj (first x) newproj0)
;				    (aset proj (second x) newproj1))) combs dists))
;	    (recur (inc iter) (/ 1 iter)))
;	(vec proj)))))
;(println "comb: " x)
;(println "proj0: " (vec proj0))
;(println "proj1 " (vec proj1))
;(println "di: " di)
;(println "dist: " y)
;(println "delta: " delta)
;(println "newproj0: " (vec newproj0))
;(println "newproj1: " (vec newproj1))

