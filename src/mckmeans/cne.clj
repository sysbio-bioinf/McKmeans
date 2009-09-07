;;;;
; File  : Cluster number estimation
; Author: Johann Kraus
;;;;

(ns mckmeans.cne
  (:use (clojure.contrib seq-utils def)
	(mckmeans utils kmeans))
;  (:import (cern.jet.random.sampling RandomSamplingAssistant)
;	   (java.io BufferedWriter FileWriter FileOutputStream OutputStreamWriter))
  (:import (assignment AssignmentProblem HungarianAlgorithm))
  (:gen-class))

(def *SAVERES* (ref false))

(defn predict-assignment
  ""
  [dat centers]
  (doall (pmap (fn [x] (whichmin (double-array (map #(distance x %) centers)))) dat)))

(defn jackknife-kmeans
  "Do jackknife resampling nrun times. Each time calculate kmeans result for the whole dataset"
  [dat nrun k maxiter]
  (let [size (int (count dat))
	nrun (int nrun)]
    (loop [run (int 0) res '()]
      (println run)
      (if (= run nrun)
	(vec res)
	(let [leaveout (. Math (ceil (. Math (sqrt size))))
	      subsetidx (sample size (- size leaveout))
	      subset (doall (map #(nth dat %1) subsetidx))
	      kmeansres (kmeans subset k maxiter)]
	  (if @*SAVERES*
	    (save-append-result "clusterresults.txt" (.. (println-str (:cluster kmeansres)) (replace "(" "") (replace ")" ""))))
	  (recur (inc run) (conj res (predict-assignment dat (:centers kmeansres)))))))))

(defn kloop-jackknife-kmeans
  "Do jackknife resampling nrun times for all k in ks"
  [dat nrun ks maxiter]
  (loop [klist ks res '()]
    (println (first klist))
    (if (empty? klist)
      (vec (reverse res))
      (recur (rest klist) (conj res (jackknife-kmeans dat nrun (first klist) maxiter))))))

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

;(init-array Float/TYPE (fn [x y] 5 5) 3 3)

(defn build-costmatrix
  ""
  [one two]
  (let [o (vec one)
	t (vec two)
	k (count (distinct o))
	max (count o)
	costMatrix (init-array Float/TYPE (fn [x y] max max) k k)]
    (dotimes [idx max]
      (let [x (nth o idx)
	    y (nth t idx)]
	(aset costMatrix x y (dec (aget costMatrix x y)))))
    costMatrix))

(defn mca-index [one two]
  (let [k (count (distinct one))
	max (count one)
	costMatrix (build-costmatrix one two)
	a (AssignmentProblem. costMatrix)
	b (. a (solve (HungarianAlgorithm.)))]
(/ (reduce + (doall (map (fn [idx] (- max (aget costMatrix (aget b idx 0) idx))) (range k)))) max)))
;    (/ (reduce + (doall (map (fn [idx] (- max (aget costMatrix idx (aget b idx 0)))) (range k)))) max)))

(defn calculate-mca-results
  "Using jackknife resampling and evaluation via MCA-index to estimate the 'right' number of clusters"
  [dat nrun ks maxiter]
  (let [jacks (kloop-jackknife-kmeans dat nrun ks maxiter)]
    (for [jack jacks]
      (flatten (for [comb (pairwise-comb nrun)] (mca-index (nth jack (first comb)) (nth jack (second comb))))))))

(defn calculate-baselines
  ""
  [dat nrun ks]
  (let [size (count dat)]
    (loop [klist ks kres '()]
      (if (empty? klist)
	(vec (reverse kres))
	(recur (rest klist) (conj kres (loop [run 0 res '()]
					 (if (= run nrun)
					   (vec res)
					   (let [centers (map #(nth dat %) (sample size (first klist)))]
					     (recur (inc run) (conj res (predict-assignment dat centers))))))))))))

(defn calculate-mca-baselines
  ""
  [dat nrun ks]
  (let [bases (calculate-baselines dat nrun ks)]
    (for [base bases]
      (flatten (for [comb (pairwise-comb nrun)] (mca-index (nth base (first comb)) (nth base (second comb))))))))

(defn get-best-k
  ""
  [clusters baselines]
  (+ 2 (whichmax (double-array (doall (map (fn [x y] (- (reduce + x) (reduce + y))) clusters baselines))))))
