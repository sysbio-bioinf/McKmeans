;;;;
; File  : multi-core kmeans cluster algorithm
; Author: Johann Kraus
;;;;

(ns mckmeans.kmeans
;;  (:use mckmeans.utils clojure.contrib.seq-utils)
  (:use mckmeans.utils clojure.contrib.seq)
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
;;  (let [sam (sample-k (count data) k)]
;(println (Thread/currentThread) "centers" sam)
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
  [point cluster-agents snp distfun]
  (if-not snp
    (if (.equals distfun "Euclidean")
      (let [dist (double-array (doall (map #(distance (:data point) (:data (deref %))) cluster-agents)))]
	(whichmin dist))
      (let [dist (double-array (doall (map #(distance-manhattan (:data point) (:data (deref %))) cluster-agents)))]
	(whichmin dist)))      
;;     (let [dist (double-array (map #(distance-snp (:data point) (:data (deref %))) cluster-agents))]
;;       (whichmin dist))))
    (let [dist (double-array (doall (map #(distance-asd (:data point) (:data (deref %))) cluster-agents)))]
      (whichmin dist))))


(defn update-datapoint
  [point cluster-agents member-refs stop snp distfun]
  (let [newass (search-best-cluster point cluster-agents snp distfun)]
    (dosync (commute stop conj (= (:assignment point) newass)))
    (dosync (commute (nth member-refs newass) conj (:data point)))
    (assoc point :assignment newass)))

(defn update-data-agent
  "A data-agent should search the best-fitting centers for its datapoints,
   change their assignment value to the corresponding cluster number,
   add their data to the corresponding cluster-members reference (commute the ref), and
   add a note whether a change was necessary to the stop (commute the ref) for each update-datapoint"
  [datalist cluster-agents member-refs stop snp distfun]
  ;(doall (pmap update-datapoint datalist))); slower
  (doall (map #(update-datapoint %1 cluster-agents member-refs stop snp distfun) datalist)))

(defn assignment
  "Tell data-agents to update. Wait until all agents finished"
  [stop data-agents cluster-agents member-refs snp distfun]
  (dosync (ref-set stop '()))
  (dorun (map #(send % (fn [x] (update-data-agent x cluster-agents member-refs stop snp distfun))) data-agents))
;;(println (Thread/currentThread) "await data-agents")
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
;;(println (Thread/currentThread) "await cluster-agents")
  (apply await cluster-agents))

;;;; KMEANS ;;;;
(defn check-stop
  "Check the stopping criteria"
  [stop curiter maxiter]
  (or (= curiter maxiter) (every? #(= true %) @stop)))

(defn read-assignments
  [data-agents]
  (doall (flatten (for [da data-agents] (map #(:assignment %) @da)))))

(defn read-centers
  [cluster-agents]
  (doall (for [ca cluster-agents] (:data @ca))))

(defn check-empty-clusters
  [member-refs]
  (reduce #(or %1 %2) (map #(empty? @%) member-refs)))

(defn kmeans
  "Run Kmeans"
  [dat k maxiter snp distfun]
;(println "start kmeans")
  (let [stop (init-stop)
	data-agents (init-data-agents dat @*NUMAGENTS*)
	cluster-agents (init-cluster-agents dat k)
	member-refs (init-member-refs k)]
    (loop [curiter 0]
;(println curiter)
      (assignment stop data-agents cluster-agents member-refs snp distfun)
			;(println (map #(empty? @%) member-refs))
			;(println (reduce #(or %1 %2) (map #(empty? @%) member-refs)))
			; if empty clusters -> restart kmeans with new clusters
      (if (check-empty-clusters member-refs)
	(do
;(println "re-init cluster centers")
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
(do
;(println (Thread/currentThread) "finished clustering")
	    (struct kmeansresult (read-assignments data-agents) (read-centers cluster-agents) curiter)
)
	    (recur (inc curiter))))))))

(defn variance-criterion [member-lists centers snpmode distfun]
  (if-not snpmode
;    (apply + (map (fn [x y]  (apply + (map #(java.lang.Math/pow (distance %1 y) 2) x))) member-lists centers))
;    (apply + (map (fn [x y]  (apply + (map #(java.lang.Math/pow (distance-asd %1 y) 2) x))) member-lists centers))))
    (if (.equals distfun "Euclidean")
      (apply + (map (fn [x y] (apply + (map (fn [u] (let [d (distance u y)] (* d d))) x))) member-lists centers))
      (apply + (map (fn [x y] (apply + (map (fn [u] (let [d (distance-manhattan u y)] (* d d))) x))) member-lists centers)))
    (apply + (map (fn [x y] (apply + (map (fn [u] (let [d (distance-asd u y)] (* d d))) x))) member-lists centers))))
  
(defn wrapper-variance-criterion [data clusterres centers snpmode distfun]
  (let [k (count centers)
	mem (loop [res (vec (replicate k nil))
		   clu clusterres
		   dat data]
	      (if (empty? clu)
		res 
		(recur (assoc res (first clu) (cons (first dat) (nth res (first clu)))) (rest clu) (rest dat))))]
    (variance-criterion mem centers snpmode distfun)))

(defn get-best-clustering
  [dat nruns k maxiter snpmode distfun]
  (loop [run 0
	 res Double/MAX_VALUE
	 clusterres nil]
    (if (= run nruns)
      [clusterres res]
      (let [kres (kmeans dat k maxiter snpmode distfun)
	    newres (double (wrapper-variance-criterion dat (:cluster kres) (:centers kres) snpmode distfun))
	    tmpres (double (if (< newres res) newres res))
	    newclusterres (if (< newres res) kres clusterres)]
;(println (Thread/currentThread) "done run" run)
	(recur (inc run) tmpres newclusterres)))))
