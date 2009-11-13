;;;;
; File  : multi-core kmeans cluster algorithm
; Author: Johann Kraus
;;;;

(ns mckmeans.kmeans
  (:use mckmeans.utils clojure.contrib.seq-utils)
  (:gen-class))

;(use '(clojure.contrib seq-utils))

(def *NUMAGENTS* (ref (.. Runtime getRuntime availableProcessors)))

(defstruct kmeansresult :cluster :centers :numruns)
(defstruct datapoint :number :data :assignment)
(defstruct cluster :number :data)

;;;; INIT ;;;;
(defn init-data-agent
  "Initialize a data agent with some datapoints - assignment of datapoints remains nil"
  [substart subend dat]
  (agent (doall (map #(struct datapoint %1 %2) (iterate inc substart) (drop substart (take subend dat))))))

(defn init-data-agents
  "Initialize a list of numagents data agents"
  [dat numagents]
  (let [len (count dat)
	end (split-seq len numagents)
	start (cons 0 (take (dec numagents) end))]
    (map #(init-data-agent %1 %2 dat) start end)))

(defn init-centers
  "Initialize centers"
  [data k]
  (let [sam (sample (count data) k)]
    (map #(nth data %) sam)))

(defn init-cluster-agent
  "Initialize a cluster agent with number and data"
  [number data]
  (agent (struct cluster number data)))

(defn init-cluster-agents
  "Initialize a list of k cluster agents"
  [dat k]
  (map #(init-cluster-agent %1 %2) (iterate inc 0) (init-centers dat k)))

(defn init-member-ref
  "Initialize a cluster member ref to nil"
  []
  (ref '()))

(defn init-member-refs
  "Initialize a list of k cluster-member refs"
  [k]
  (map (fn [x] (init-member-ref)) (range k)))

(defn init-stop
  "Initialize stop to nil"
  []
  (ref '()))

;;;; ASSIGNMENT ;;;;
(defn search-best-cluster
  "Search the number of the nearest center"
  [point cluster-agents snp]
  (if-not snp
    (let [dist (double-array (map #(distance (:data point) (:data (deref %))) cluster-agents))]
      (whichmin dist))
    (let [dist (double-array (map #(distance-snp (:data point) (:data (deref %))) cluster-agents))]
      (whichmin dist))))


(defn update-datapoint
  [point cluster-agents member-refs stop snp]
  (let [newass (search-best-cluster point cluster-agents snp)]
    (dosync (commute stop conj (= (:assignment point) newass)))
    (dosync (commute (nth member-refs newass) conj (:data point)))
    (assoc point :assignment newass)))

(defn update-data-agent
  "A data-agent should search the best-fitting centers for its datapoints,
   change their assignment value to the corresponding cluster number,
   add their data to the corresponding cluster-members reference (commute the ref), and
   add a note whether a change was necessary to the stop (commute the ref) for each update-datapoint"
  [datalist cluster-agents member-refs stop snp]
  ;(doall (pmap update-datapoint datalist))); slower
  (doall (map #(update-datapoint %1 cluster-agents member-refs stop snp) datalist)))

(defn assignment
  "Tell data-agents to update. Wait until all agents finished"
  [stop data-agents cluster-agents member-refs snp]
  (dosync (ref-set stop '()))
  (dorun (map #(send % (fn [x] (update-data-agent x cluster-agents member-refs stop snp))) data-agents))
  (apply await data-agents))

;;;; UPDATE ;;;;
(defn update-centerpoint [members snp]
  (if-not snp
    (let [scale (double (count members))
	  #^doubles newcen (reduce da+ members)]
      (amap newcen i ret
	    (/ (aget newcen i) scale)))
    (let [#^ints newcen (reduce #(da-snp-freq %1 %2) (int-array (* 3 (alength (first members)))) members)]
      (da-snp-mode newcen))))

(defn update-cluster-agent
  "A cluster-agent should calculate its new center from its corresponding cluster-members,
   change its data vector to the new center, and
   clear cluster-members"
  [clus member-refs snp]
  (let [mem (deref (nth member-refs (:number clus)))
	newcen (update-centerpoint mem snp)]
    (dosync (ref-set (nth member-refs (:number clus)) '()))
    (assoc clus :data newcen)))

(defn update
  "Tell cluster-agents to update. Wait until all agents finished"
  [cluster-agents member-refs snp]
  (dorun (map #(send % (fn [x] (update-cluster-agent x member-refs snp))) cluster-agents))
  (apply await cluster-agents))

;;;; KMEANS ;;;;
(defn check-stop
  "Check the stopping criteria"
  [stop curiter maxiter]
  (or (= curiter maxiter) (every? #(= true %) @stop)))

(defn read-assignments
  [data-agents]
  (flatten (for [da data-agents] (map #(:assignment %) @da))))

(defn read-centers
  [cluster-agents]
  (for [ca cluster-agents] (:data @ca)))

(defn check-empty-clusters
  [member-refs]
  (reduce #(or %1 %2) (map #(empty? @%) member-refs)))

(defn kmeans
  "Run Kmeans"
  [dat k maxiter snp]
  (let [stop (init-stop)
	data-agents (init-data-agents dat @*NUMAGENTS*)
	cluster-agents (init-cluster-agents dat k)
	member-refs (init-member-refs k)]
    (loop [curiter 0]
      (assignment stop data-agents cluster-agents member-refs snp)
			;(println (map #(empty? @%) member-refs))
			;(println (reduce #(or %1 %2) (map #(empty? @%) member-refs)))
			; if empty clusters -> restart kmeans with new clusters
      (if (check-empty-clusters member-refs)
	(do
;					(println "hier")
					; re-init cluster centers
	  (dorun (map (fn [x y] (send x (fn [z w] (deref w)) y)) cluster-agents (init-cluster-agents dat k)))
	  (apply await cluster-agents)
;					(println cluster-agents)
	  (doall (map #(dosync (ref-set % '())) member-refs))
	  (recur 0))

	(do
;					(println "da")
	  (update cluster-agents member-refs snp)
	  (if (check-stop stop curiter maxiter)
	    (struct kmeansresult (read-assignments data-agents) (read-centers cluster-agents) curiter)
	    (recur (inc curiter))))))))
