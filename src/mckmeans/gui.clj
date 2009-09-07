;;;;
; File  : GUI for McKmeans application
; Author: Johann Kraus
;;;;

(ns mckmeans.gui
  #^{:author "Johann M. Kraus",
     :doc "Multi-core kmeans cluster application"}
;  (:refer-clojure)
  (:use (mckmeans kmeans utils cne))
  (:import (javax.swing JFrame JLabel JButton JPanel JMenuBar JMenu JMenuItem JFileChooser JTextField JCheckBox JTextArea JScrollPane)
	   (java.awt.event ActionListener KeyListener)
	   (java.awt FlowLayout GridLayout BorderLayout Color)
	   (java.io BufferedWriter FileWriter FileOutputStream OutputStreamWriter)
	   (org.jfree.data.xy DefaultXYDataset)
	   (org.jfree.chart.axis CategoryAnchor AxisLocation)
	   (org.jfree.data.statistics DefaultBoxAndWhiskerCategoryDataset)
	   (org.jfree.chart ChartFactory JFreeChart ChartPanel)
	   (org.jfree.chart.plot PlotOrientation)
	   (org.apache.batik.dom GenericDOMImplementation)
	   (org.apache.batik.svggen SVGGraphics2D)
	   (org.w3c.dom DOMImplementation Document))
  (:gen-class))

;;;; GLOBALS ;;;;
(def *DATASET* (ref nil))
(def *RESULT* (ref nil))
(def *K* (ref 2))
(def *MAXITER* (ref 10))
(def *ERUNS* (ref 10))
(def *DIMX* (ref 0))
(def *DIMY* (ref 1))


;### only 2 dim plots; TODO improve via PCA
;(defn data2plotdata [data]
;  (let [numrows (count data) xy (make-array (. Double TYPE) 2 numrows)]
;    (dotimes [idx numrows]
;			(aset-double (aget xy 0) idx (first (nth data idx)))
;      (aset-double (aget xy 1) idx (second (nth data idx))))
;    xy))

;(defn members2plotdata [dimx dimy mem]
;  (let [numrows (count (deref mem))
;				data (make-array (. Double TYPE) 2 numrows)]
;    (dotimes [idx numrows]
;			(aset-double (aget data 0) idx (nth (nth (deref mem) idx) dimx))
;      (aset-double (aget data 1) idx (nth (nth (deref mem) idx) dimy)))
;    data))

(defn data2plotdata [dimx dimy]
  (let [numrows (count @*DATASET*)
				data (make-array (. Double TYPE) 2 numrows)]
    (dotimes [idx numrows]
			(aset-double (aget data 0) idx (nth (nth @*DATASET* idx) dimx))
      (aset-double (aget data 1) idx (nth (nth @*DATASET* idx) dimy)))
    (list data)))

;(defn make-plotdata [dimx dimy]
;	(if (= member-refs nil)
;		(list (data2plotdata dimx dimy))
;		(map #(members2plotdata dimx dimy %) member-refs)))

(defn assignments2plotdata [dimx dimy ass]
	(let [data (map (fn [x] (make-array (. Double TYPE) 2 (count (filter #(= x %) ass)))) (range @*K*))
				ass (vec ass)
				maxidx (count ass)]
		(loop [idx 0 idxs (replicate @*K* 0)]
			(if (< idx maxidx)
				(let [curidx (nth ass idx)
							curdata (nth data curidx)
							curdataidx (nth idxs curidx)]
					(aset-double (aget curdata 0) curdataidx (nth (nth @*DATASET* idx) dimx))
					(aset-double (aget curdata 1) curdataidx (nth (nth @*DATASET* idx) dimy))
					(recur (inc idx) (map (fn [x y] (if (= curidx y) (inc x) x)) idxs (iterate inc 0))))
				data))))

(defn make-plotdata [dimx dimy result]
  (if (= result nil)
    (data2plotdata dimx dimy)
    (assignments2plotdata dimx dimy (:cluster result))))

(defn show-options-panel
  ""
  []
  (let [options-panel (JPanel.)
	options-frame (JFrame. "Options")
	dim-label (new JLabel " Select features to plot:")
	dimx-label (new JLabel " dim x:")
	dimy-label (new JLabel " dim y:")
	dimx-text (new JTextField)
	dimy-text (new JTextField)
	maxiter-text (new JTextField)
	maxiter-label (new JLabel "Maximal number of iterations:")
	estimate-label (new JLabel "Cluster number estimation parameters:")
	estimate-run-label (new JLabel "Number of resamplings per cluster:")
	estimate-run-text (new JTextField)
	parameter-label (new JLabel "K-means parameters:")
	save-estimation (new JCheckBox "Save cluster results" false)
	update-button (new JButton "Update")]

    (. update-button
       (addActionListener
        (proxy [ActionListener] []
	  (actionPerformed [evt]
			   (let [eruns (try (. Integer (parseInt (. estimate-run-text (getText)))) (catch Exception e @*ERUNS*))
				 maxiter (try (. Integer (parseInt (. maxiter-text (getText)))) (catch Exception e @*MAXITER*))
				 dimx (try (. Integer (parseInt (. dimx-text (getText)))) (catch Exception e @*DIMX*))
				 dimy (try (. Integer (parseInt (. dimy-text (getText)))) (catch Exception e @*DIMY*))
				 saveres (not (nil? (. save-estimation (getSelectedObjects))))]
			     (dosync (ref-set *DIMX* dimx))
			     (dosync (ref-set *DIMY* dimy))
			     (dosync (ref-set *MAXITER* maxiter))
			     (dosync (ref-set *ERUNS* eruns))
			     (dosync (ref-set *SAVERES* saveres)))
			   (. options-frame (dispose))))))

    (. dimx-text (setText (pr-str @*DIMX*)))
    (. dimy-text (setText (pr-str @*DIMY*)))
    (. maxiter-text (setText (pr-str @*MAXITER*)))
    (. estimate-run-text (setText (pr-str @*ERUNS*)))
    (. save-estimation (setSelected @*SAVERES*))
    
    (doto options-panel
      (. setLayout (new GridLayout 7 3 5 5))
      
      (. add dim-label)
      (. add dimx-label)
      (. add dimy-label)
                        ;(. add (new JLabel ""))
      (. add (new JLabel ""))
      (. add dimx-text)
      (. add dimy-text)
                        ;(. add plot-button)

      (. add parameter-label)
                        ;(. add numcluster-label)
      (. add maxiter-label)
      (. add (new JLabel ""))
      (. add (new JLabel ""))
                        ;(. add numcluster-text)
      (. add maxiter-text)
      (. add (new JLabel ""))
                        ;(. add run-button)

      (. add estimate-label)
                                        ;(. add estimate-k-label)
      (. add estimate-run-label)
      (. add (new JLabel ""))
      (. add (new JLabel ""))
                        ;(. add estimate-k-text)
      (. add estimate-run-text)
                        ;(. add estimate-button))
      (. add (new JLabel ""))
      (. add save-estimation)
      (. add (new JLabel ""))
      (. add update-button))

    (doto options-frame
      (. setSize 400 400)
      (. setLayout (new BorderLayout))
      (. add options-panel)
      (. pack)
      (. setVisible true))))

(defn exportChart2SVG
  ""
  [chart frame filename]
  (let [bound (.. frame (getContentPane) (getBounds))
	domImpl (. GenericDOMImplementation (getDOMImplementation))
	document (. domImpl (createDocument nil, "svg", nil))
	svgGenerator (SVGGraphics2D. document)
	outputStream (FileOutputStream. filename)
	out (OutputStreamWriter. outputStream "UTF-8")]

    (. chart (draw svgGenerator bound))
    (. svgGenerator (stream out true))
    (. outputStream (flush))
    (. outputStream (close))))

(defn save-clusternumber-result [results baselines filename]
;  (let [res (str (doall (map #(str (println-str %3 %1) (println-str %3 %2)) results baselines (iterate inc 2))))]
   (let [res (.. (pr-str (doall (map #(list (list %3 %1) (list %3 %2)) results baselines (iterate inc 2)))) (replace ") (" "\n") (replace "(" "") (replace ")" ""))]
     (save-result filename res)))

(defn boxplot-clusternumber [results baselines]
  (let [dat (DefaultBoxAndWhiskerCategoryDataset.)
	frame (JFrame. "Cluster number estimation")
	chart (org.jfree.chart.ChartFactory/createBoxAndWhiskerChart "" "Cluster number" "MCA-index" dat true)
	plot-panel (new ChartPanel chart)
	len (count results)
	
	menubar (new JMenuBar)
	menu-file (new JMenu "File")
	menu-save (new JMenuItem "Save")
	menu-export-svg (new JMenuItem "Export (SVG)")

	file-chooser (new JFileChooser)]

    (do (.. chart getCategoryPlot getRenderer (setMaximumBarWidth 0.25))
	(.. chart getCategoryPlot getRenderer (setFillBox true))
	(.. chart getCategoryPlot getRenderer (setMeanVisible false))
;	(.. chart getCategoryPlot (setDomainAxisLocation 2 AxisLocation/BOTTOM_OR_LEFT))

	(.. chart getCategoryPlot getRangeAxis (setRange 0.0 1.05))

;	(.. chart getCategoryPlot getDomainAxis (setLowerMargin 0.10))
	(.. chart getCategoryPlot getDomainAxis (setCategoryMargin 0.4))

	(.. chart getCategoryPlot (setDomainGridlinePosition CategoryAnchor/MIDDLE))


	(.. chart getCategoryPlot (setDomainGridlinesVisible true))
;	(.. chart getCategoryPlot (setAnchorValue 1.5))


	(dorun (map #(.add dat %1 %2 %3) results (replicate len "McKmeans result") (iterate inc 2)))
	(dorun (map #(.add dat %1 %2 %3) baselines (replicate len "Random prototype baseline") (iterate inc 2)))

	(. menu-export-svg
	   (addActionListener
	    (proxy [ActionListener] []
	      (actionPerformed [evt]
			       (try
				(let [ret (. file-chooser (showSaveDialog frame))
				      filename (. (. file-chooser (getSelectedFile)) (getPath))]
				  (exportChart2SVG chart frame filename))
				(catch Exception e nil))))))

	(. menu-save
	   (addActionListener
	    (proxy [ActionListener] []
	      (actionPerformed [evt]
			       (try
				(let [ret (. file-chooser (showSaveDialog frame))
				      filename (. (. file-chooser (getSelectedFile)) (getPath))]
				  (save-clusternumber-result results baselines filename))
				(catch Exception e nil))))))
				
	(doto menu-file
	  (. add menu-save)
	  (. add menu-export-svg))

	(doto menubar (. add menu-file))

	(doto frame
	  (. setSize 400 400)
	  (. setJMenuBar menubar)
	  (. setLayout (new BorderLayout))
	  (. add plot-panel)
	  (. pack)
	  (. setVisible true))) ))
    
    ;;(doall (map #(- (.getMediaVailue dat "McKmeans result" %) (.getMedianValue dat "Random prototype baseline" %)) (iterate inc 2)))))

;###
(defn runGUI []
  (let [frame (new JFrame "McKmeans")
	menubar (new JMenuBar)
	menu-file (new JMenu "File")
	menu-file-load (new JMenuItem "Load")
	menu-file-save (new JMenuItem "Save")
	menu-export-svg (new JMenuItem "Export (SVG)")
                                
	menu-options (new JMenu "Options")
	menu-options-clusters (new JMenuItem "Cluster options")

	menu-help (new JMenu "Help")
	menu-help-about (new JMenuItem "About")

	plot-data (new DefaultXYDataset)
	plot-area (. ChartFactory (createScatterPlot "" "x-Axis" "y-Axis" plot-data (. PlotOrientation VERTICAL) true false false))
	plot-panel (new ChartPanel plot-area)

	work-panel (new JPanel)
	;plot-button (new JButton "Plot")
	run-button (new JButton "Run clustering")
	run-button-panel (JPanel.)
	estimation-button-panel (JPanel.)

	cluster-panel (new JPanel)
	estimation-panel (new JPanel)

	numcluster-text (new JTextField)
	numcluster-label (new JLabel "Number of clusters:")
;	maxiter-text (new JTextField)
;	maxiter-label (new JLabel "Maximal number of iterations:")
	result-label (new JLabel " Resulting cluster assignments:")
	result-text (new JTextArea 10 1)
	result-scrollpane (JScrollPane. result-text)
;	dim-label (new JLabel " Select features to plot:")
;	dimx-label (new JLabel " dim x:")
;	dimy-label (new JLabel " dim y:")
;	dimx-text (new JTextField)
;	dimy-text (new JTextField)
;	parameter-label (new JLabel "Change k-means parameters:")

;	estimate-label (new JLabel " Change cluster number estimation parameters:")
	estimate-k-label (new JLabel " Maximal number of clusters:")
;	estimate-run-label (new JLabel " Number of resamplings per cluster:")
	estimate-k-text (new JTextField)
;	estimate-run-text (new JTextField)
	estimate-button (new JButton "Run cluster number estimation")

	result-panel (new JPanel)

	statusbar (new JLabel " Welcome to the McKmeans cluster application ...")
	file-chooser (new JFileChooser)]

		(. result-text (setLineWrap true))

    (. statusbar (setForeground (. Color red)))
;		(. statusbar (setBackground (. Color blue)))
    (. numcluster-text (setText (pr-str @*K*)))
;    (. maxiter-text (setText (pr-str @*MAXITER*)))
;    (. dimx-text (setText "0"))
;    (. dimy-text (setText "1"))
    (. estimate-k-text (setText "10"))
;    (. estimate-run-text (setText "20"))


;		(. numcluster-text
;			 (addActionListener
;				(proxy [ActionListener] []
;					(actionPerformed [evt]
;													 (dosync (ref-set *K* (. Integer (parseInt (. numcluster-text (getText))))))))))

;		(. numcluster-text
;			 (addKeyListener
;				(proxy [KeyListener] []
;					(keyTyped [evt]
;										(let [text (. numcluster-text (getText))]
;											(if (not (= text ""))
;												(dosync (ref-set *K* (. Integer (parseInt text)))))))
;					(keyPressed [evt])
;					(keyReleased [evt]))))

;		(. maxiter-text
;			 (addActionListener
;				(proxy [ActionListener] []
;					(actionPerformed [evt]
;													 (dosync (ref-set *MAXITER* (. Integer (parseInt (. maxiter-text (getText))))))))))

;	(. maxiter-text
;	   (addKeyListener
;	    (proxy [KeyListener] []
;				(keyTyped [evt]
;				    (dosync (ref-set *MAXITER* (. Integer (parseInt (. maxiter-text (getText)))))))
;				(keyPressed [evt])
;				(keyReleased [evt]))))

    (. run-button
       (addActionListener
	(proxy [ActionListener] []
	       (actionPerformed [evt]
				(. statusbar (setText " running cluster analysis ... this may take some time ..."))

;TODO move this to actionlisteners on the text fields
				      ;(dosync (ref-set *K* (. Integer (parseInt (. numcluster-text (getText))))))
				      ;(dosync (ref-set *MAXITER* (. Integer (parseInt (. maxiter-text (getText))))))
;

				(dosync (ref-set *K* (try (. Integer (parseInt (. numcluster-text (getText)))) (catch Exception e @*K*))))
				(. numcluster-text (setText (pr-str @*K*)))
				;(dosync (ref-set *MAXITER* (. Integer (parseInt (. maxiter-text (getText))))))
				(let [res (kmeans @*DATASET* @*K* @*MAXITER*)
				      old (. plot-data (getSeriesCount))]

;(println "done -> plotting")

				  (dosync (ref-set *RESULT* res))	  
				  (doall (map (fn [idx] (doto plot-data (. removeSeries (str "cluster " idx)))) (drop 1 (range (inc old)))))
				  (doall (map (fn [idx x] (doto plot-data (. addSeries (str "cluster " idx) x))) (iterate inc 1) (make-plotdata @*DIMX* @*DIMY* @*RESULT*))))
				(let [restext (.. (pr-str (:cluster @*RESULT*)) (replace "(" "") (replace ")" ""))]
					(. result-text
				   (setText restext)))
				(. statusbar (setText " finished clustering"))))))

    (. estimate-button
       (addActionListener
	(proxy [ActionListener] []
	  (actionPerformed [evt]
			   (. statusbar (setText " running cluster number estimation ... this will take some time ..."))
			   (let [ks-input (try (. Integer (parseInt (. estimate-k-text (getText)))) (catch Exception e 10))
				 ks (drop 2 (range (inc ks-input)))
				 clusterresults (calculate-mca-results @*DATASET* @*ERUNS* ks @*MAXITER*)
				 baselineresults (calculate-mca-baselines @*DATASET* @*ERUNS* ks)
				 bestk (get-best-k clusterresults baselineresults)
				 res (kmeans @*DATASET* bestk @*MAXITER*)
				 old (. plot-data (getSeriesCount))]
			     (boxplot-clusternumber clusterresults baselineresults)
;(println "done -> plotting")

			     (. estimate-k-text (setText (pr-str ks-input)))
				  
			     (dosync (ref-set *K* bestk))
			     (dosync (ref-set *RESULT* res))
			     (. numcluster-text (setText (pr-str bestk)))
			     (let [restext (.. (pr-str (:cluster @*RESULT*)) (replace "(" "") (replace ")" ""))]
			       (. result-text
				  (setText restext)))
			     
			     (. statusbar (setText " finished cluster number estimation"))
			     
			     (doall (map (fn [idx] (doto plot-data (. removeSeries (str "cluster " idx)))) (drop 1 (range (inc old)))))				 
			     (doall (map (fn [idx x] (doto plot-data (. addSeries (str "cluster " idx) x))) (iterate inc 1) (make-plotdata @*DIMX* @*DIMY* @*RESULT*))))))))

;		(. plot-button
;			 (addActionListener
;				(proxy [ActionListener] []
;					(actionPerformed [evt]
;													 (let [xy (data2plotdata @*DATASET*)]
;														 (doto plot-data
;															 (. addSeries "cluster one" xy)))))))

		;(. plot-button
		;   (addActionListener
		;    (proxy [ActionListener] []
		;	   (actionPerformed [evt]
		;			    (let [xy (make-plotdata (. Integer (parseInt (. dimx-text (getText)))) (. Integer (parseInt (. dimy-text (getText)))) @*RESULT*)
		;				  old (. plot-data (getSeriesCount))]
		;			      (doall (map (fn [idx] (doto plot-data (. removeSeries (str "cluster " idx)))) (drop 1 (range (inc old)))))
		;			      (doall (map (fn [idx x] (doto plot-data (. addSeries (str "cluster " idx) x))) (iterate inc 1) xy)))))))

		;(doto work-panel
			;(. setLayout (new GridLayout 6 4 5 5))
			
			;(. add dim-label)
			;(. add dimx-label)
			;(. add dimy-label)
			;(. add (new JLabel ""))
			;(. add (new JLabel ""))
			;(. add dimx-text)
			;(. add dimy-text)
			;(. add plot-button)

			;(. add parameter-label)
			;(. add numcluster-label)
			;(. add maxiter-label)
			;(. add (new JLabel ""))
			;(. add (new JLabel ""))
			;(. add numcluster-text)
			;(. add maxiter-text)
			;(. add run-button)

			;(. add estimate-label)
			;(. add estimate-k-label)
			;(. add estimate-run-label)
			;(. add (new JLabel ""))
			;(. add (new JLabel ""))
			;(. add estimate-k-text)
			;(. add estimate-run-text)
			;(. add estimate-button))

		(doto cluster-panel
		  (. setLayout (new FlowLayout FlowLayout/LEFT 35 5))
		  (. add numcluster-label)
		  (. add numcluster-text))
		  ;(. add run-button))
			
		(doto estimation-panel
		  (. setLayout (new FlowLayout FlowLayout/LEFT))
		  (. add estimate-k-label)
		  (. add estimate-k-text))
		  ;(. add estimate-button))

		(doto run-button-panel
			(. setLayout (new FlowLayout FlowLayout/LEFT))
			(. add run-button))

		(doto estimation-button-panel
			(. setLayout (new FlowLayout FlowLayout/LEFT))
			(. add estimate-button))

		(doto work-panel
		  (. setLayout (new GridLayout 2 2 5 5))
		  (. add cluster-panel)
			(. add run-button-panel)
		  (. add estimation-panel)
		  (. add estimation-button-panel))

		(doto result-panel
		  (. setLayout (new BorderLayout))
		  ;(. add result-label (. BorderLayout NORTH))
		  ;(. add result-scrollpane (. BorderLayout CENTER))
		  (. add statusbar (. BorderLayout SOUTH)))

		(. menu-file-load
		   (addActionListener
		    (proxy [ActionListener] []
		      (actionPerformed [evt]
				       (try
					(let [ret (. file-chooser (showOpenDialog frame))
					      filename (. (. file-chooser (getSelectedFile)) (getPath))
					      dataset (load-tab-file filename)
					      old (. plot-data (getSeriesCount))]
					  (dosync (ref-set *DATASET* dataset))
					  (doall (map (fn [idx] (doto plot-data (. removeSeries (str "cluster " idx)))) (drop 1 (range (inc old)))))
					  (doall (map (fn [idx x] (doto plot-data (. addSeries (str "cluster " idx) x))) (iterate inc 1) (data2plotdata @*DIMX* @*DIMY*)))
					  (. result-text (setText ""))
					  (. statusbar
					     (setText " file loaded")))
					(catch Exception e (. statusbar (setText " error while loading file"))))
				       ))))

		(. menu-file-save
		   (addActionListener
		    (proxy [ActionListener] []
		      (actionPerformed [evt]
				       (try
					(let [ret (. file-chooser (showSaveDialog frame))
					      filename (. (. file-chooser (getSelectedFile)) (getPath))
					      restext (.. (pr-str (:cluster @*RESULT*)) (replace "(" "") (replace ")" ""))]
					  (save-result filename restext)
					  (. statusbar
					     (setText " result saved")))
					(catch Exception e (. statusbar (setText " error while saving file"))))
				       ))))

		(. menu-export-svg
		   (addActionListener
		    (proxy [ActionListener] []
		      (actionPerformed [evt]
				       (try
					(let [ret (. file-chooser (showSaveDialog frame))
					      filename (. (. file-chooser (getSelectedFile)) (getPath))]
					  (exportChart2SVG plot-area frame filename))
					(catch Exception e nil))))))
		
		(doto menu-file
		  (. add menu-file-load)
		  (. add menu-file-save)
		  (. add menu-export-svg))

		(. menu-options-clusters
		  (addActionListener
		   (proxy [ActionListener] []
			  (actionPerformed [evt]
					   (show-options-panel)))))

		(doto menu-options
		  (. add menu-options-clusters))

		(. menu-help-about
		   (addActionListener
		    (proxy [ActionListener] []
			   (actionPerformed [evt]
					    (. statusbar (setText " about - not working yet"))))))
	
		(doto menu-help
		  (. add menu-help-about))
		
		(doto menubar
			(. add menu-file)
			(. add menu-options)
			(. add menu-help))
	
		(doto frame
			(. setSize 1000 1000)
			(. setJMenuBar menubar)
			(. setLayout (new BorderLayout))
			(. add plot-panel (. BorderLayout NORTH))
			(. add work-panel (. BorderLayout CENTER))
			(. add result-panel (. BorderLayout SOUTH))

			(. pack)
			(. setDefaultCloseOperation (. JFrame EXIT_ON_CLOSE)) ;uncomment this line to quit stand-alone app on frame close
			(. setVisible true))))




(defn -main [& args] (runGUI))
