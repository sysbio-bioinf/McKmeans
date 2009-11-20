;;;;
; File  : GUI for McKmeans application
; Author: Johann Kraus
;;;;

(ns mckmeans.gui
  #^{:author "Johann M. Kraus",
     :doc "Multi-core kmeans cluster application"}
  (:use (mckmeans kmeans utils cne)
	clojure.contrib.command-line)
  (:import (javax.swing JFrame JLabel JButton JPanel JMenuBar JMenu JMenuItem JFileChooser JTextField JCheckBox JTextArea JScrollPane JTabbedPane JOptionPane SwingUtilities GroupLayout JEditorPane)
	   (javax.swing.filechooser FileFilter)
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

;;;; GLOBALS ;;;;
(def *TRANSPOSED* (ref false))
(def *TDATASET* (ref nil))
(def *DATASET* (ref nil))
(def *RESULT* (ref nil))
(def *K* (ref 2))
(def *MAXITER* (ref 10))
(def *ERUNS* (ref 10))
(def *CNEMAX* (ref 10))
(def *DIMX* (ref 0))
(def *DIMY* (ref 1))
(def *SNPMODE* (ref false))

;### only 2 dim plots; TODO improve via PCA
(defn data2plotdata [dataset dimx dimy]
  (let [numrows (count dataset)
	data (make-array (. Double TYPE) 2 numrows)]
    (dotimes [idx numrows]
      (aset-double (aget data 0) idx (nth (nth dataset idx) dimx))
      (aset-double (aget data 1) idx (nth (nth dataset idx) dimy)))
    (list data)))

(defn data2snpplotdata [dataset snpnum]
  (let [numrows (count dataset)
	len (alength (nth dataset snpnum))
	data (make-array (. Double TYPE) 2 len)]
    (dotimes [idx len]
      (aset-double (aget data 0) idx (inc idx))
      (aset-double (aget data 1) idx (aget (nth dataset snpnum) idx)))
    data))

(defn make-snpplotdata
  [dataset snps]
  (map #(data2snpplotdata dataset %) snps))

(defn assignments2plotdata [dataset dimx dimy ass]
  (let [data (map (fn [x] (make-array (. Double TYPE) 2 (count (filter #(= x %) ass)))) (range @*K*))
	ass (vec ass)
	maxidx (count ass)]
    (loop [idx 0 idxs (replicate @*K* 0)]
      (if (< idx maxidx)
	(let [curidx (nth ass idx)
	      curdata (nth data curidx)
	      curdataidx (nth idxs curidx)]
	  (aset-double (aget curdata 0) curdataidx (nth (nth dataset idx) dimx))
	  (aset-double (aget curdata 1) curdataidx (nth (nth dataset idx) dimy))
	  (recur (inc idx) (map (fn [x y] (if (= curidx y) (inc x) x)) idxs (iterate inc 0))))
	data))))

(defn make-plotdata [dataset dimx dimy result]
  (if (= result nil)
    (data2plotdata dataset dimx dimy)
    (assignments2plotdata dataset dimx dimy (:cluster result))))

(defn create-kmodes-cluster-plot [clusterdata]
  (let [tmp-data (DefaultXYDataset.)
	cluster-plot (. (. ChartFactory (createXYLineChart "" "" "SNP code" tmp-data (. PlotOrientation VERTICAL) false false false)) (getXYPlot))] ; refactor this line !!!
    (doall (map (fn [idx x] (doto tmp-data (. addSeries idx x))) (iterate inc 0) (make-snpplotdata clusterdata (range (count clusterdata)))))
    (.. cluster-plot getRangeAxis (setStandardTickUnits (NumberAxis/createIntegerTickUnits)))
    (.. cluster-plot getDomainAxis (setStandardTickUnits (NumberAxis/createIntegerTickUnits)))
    (.. cluster-plot getRangeAxis (setRange -0.5 2.5))
   
    cluster-plot))

;; about panel
(defn show-about-panel
  []
  (let [about-panel (JPanel.)
	about-frame (JFrame. "About")
	about-text (JTextArea. 5 1)
	close-button (JButton. "Close")]
    (. about-text (setEditable false))
    (. about-text (setText "\tMcKmeans\n\nThis is the mulit-core K-means cluster\napplication. Perform K-means cluster\nanalysis and cluster number\nestimation while using all\nyour hardware.\n\nVersion:\t0.42\nAuthors:\tJohann M. Kraus\n\tHans A. Kestler\nCopyright:\tArtistic Licence 2.0"))
    (. close-button
      (addActionListener
       (proxy [ActionListener] []
	 (actionPerformed [evt]
			  (. about-frame (dispose))))))

    (doto about-panel
      (. setLayout (BorderLayout.))
      (. add about-text BorderLayout/CENTER)
      (. add close-button BorderLayout/SOUTH))

    (doto about-frame
      (. setSize 200 200)
      (. setLayout (new BorderLayout))
      (. add about-panel)
      (. pack)
      (. setVisible true))))

;; help panel
(defn show-help-panel
  []
  (let [editor-panel (JEditorPane.)
	editor-frame (JFrame. "Help")]
    (. editor-panel setEditable false)
; (URL. "file:///......index.html")
    (. editor-panel setText "index.html")))

;; options panel
(defn show-preferences-panel
  []
    (let [options-panel (JPanel.)
	options-frame (JFrame. "Preferences")
	dim-label (new JLabel " Select features to plot:")
	dimx-label (new JLabel " dim x:")
	dimy-label (new JLabel " dim y:")
	dimx-text (new JTextField)
	dimy-text (new JTextField)
	save-estimation (new JCheckBox "" false)
	update-button (new JButton "Update")]

    (. update-button
       (addActionListener
        (proxy [ActionListener] []
	  (actionPerformed [evt]
			   (let [dimx (try (. Integer (parseInt (. dimx-text (getText)))) (catch Exception e @*DIMX*))
				 dimy (try (. Integer (parseInt (. dimy-text (getText)))) (catch Exception e @*DIMY*))
				 saveres (not (nil? (. save-estimation (getSelectedObjects))))]
			     (dosync (ref-set *DIMX* dimx))
			     (dosync (ref-set *DIMY* dimy))
			     (dosync (ref-set *SAVERES* saveres)))
			   (. options-frame (dispose))))))

    (. dimx-text (setText (pr-str @*DIMX*)))
    (. dimy-text (setText (pr-str @*DIMY*)))
    (. save-estimation (setSelected @*SAVERES*))
    
    (doto options-panel
      (. setLayout (new GridLayout 4 3 5 5))
      
      (. add dim-label)
      (. add dimx-label)
      (. add dimy-label)

      (. add (new JLabel ""))
      (. add dimx-text)
      (. add dimy-text)

      (. add (new JLabel "Save all results from cluster number estimation?"))
      (. add save-estimation)
      (. add (new JLabel ""))

      (. add (new JLabel ""))
      (. add (new JLabel ""))
      (. add update-button))

    (doto options-frame
      (. setSize 400 400)
      (. setLayout (new BorderLayout))
      (. add options-panel)
      (. pack)
      (. setVisible true))))


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
	menu-file-load (new JMenuItem "Load data")
	menu-file-save (new JMenuItem "Save clustering")
	menu-export-svg (new JMenuItem "Export graphic to SVG")
	
	menu-options (new JMenu "Options")
	menu-options-clusters (new JMenuItem "Cluster options")

	menu-preferences (JMenuItem. "Preferences...")
	menu-exit (JMenuItem. "Exit")

	menu-help (new JMenu "Help")
	menu-help-about (new JMenuItem "About")

	tabbed-pane (JTabbedPane.)

	kmeans-panel (JPanel.)
	kmodes-panel (JPanel.)
	cne-panel(JPanel.)

	kmeans-options-panel (JPanel.)
	kmodes-options-panel (JPanel.)
	cne-options-panel (JPanel.)

	plot-data (new DefaultXYDataset)
	plot-area (. ChartFactory (createScatterPlot "" "dim x" "dim y" plot-data (. PlotOrientation VERTICAL) true false false))
	plot-panel (new ChartPanel plot-area)

	kmodes-data (DefaultXYDataset.)
	kmodes-chart (. ChartFactory (createXYLineChart "" "" "SNP code" kmodes-data (. PlotOrientation VERTICAL) true false false))
	kmodes-combined-plot (CombinedDomainXYPlot.)
	kmodes-combined-chart (JFreeChart. kmodes-combined-plot)
;	kmodes-data (DefaultCategoryDataset.)
;	kmodes-chart (ChartFactory/createLineChart "" "feature" "sample" kmodes-data (. PlotOrientation VERTICAL) true false false)
;	kmodes-plot-panel (ChartPanel. kmodes-chart)

	boxplot-data (DefaultBoxAndWhiskerCategoryDataset.)
	boxplot-chart (org.jfree.chart.ChartFactory/createBoxAndWhiskerChart "" "cluster k" "MCA-index" boxplot-data true)
	boxplot-panel (ChartPanel. boxplot-chart)


	work-panel (new JPanel)
	;plot-button (new JButton "Plot")
	run-button (new JButton "Cluster!")
	run-button-panel (JPanel.)
	estimation-button-panel (JPanel.)

	numcluster-kmodes-text (new JTextField)
	numcluster-kmodes-label (new JLabel " Number of clusters k:")
	maxiter-kmodes-text (new JTextField)
	maxiter-kmodes-label (new JLabel " Maximal number of iterations:")	
	run-kmodes-button (new JButton " Run Clustering")

	cluster-panel (new JPanel)
	estimation-panel (new JPanel)

	numcluster-text (new JTextField 7)
	numcluster-label (new JLabel " Number of clusters k:")
	maxiter-text (new JTextField 7)
	maxiter-label (new JLabel " Maximal number of iterations:")
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
	estimate-k-label (new JLabel " Maximal number of clusters k:")
	estimate-run-label (new JLabel " Number of resamplings per cluster:")
	estimate-k-text (new JTextField 7)
	estimate-run-text (new JTextField 7)
	estimate-button (new JButton "Run cluster number estimation!")
	estimate-maxiter-label (JLabel. " Maximal number of iterations:")
	estimate-maxiter-text (JTextField. 7)

	result-panel (new JPanel)

	statusbar (new JLabel " Welcome to the McKmeans cluster application ...")
	file-chooser (new JFileChooser)
	
	info-panel (JPanel.)
	info-sample-text (JTextField. 7)
	info-feature-text (JTextField. 7)
	info-sample-label (JLabel. "Number of samples:")
	info-feature-label (JLabel. "Number of features:")
	info-swap-button (JButton. "Transpose data!")]

    (. info-sample-text (setBackground Color/lightGray))
    (. info-feature-text (setBackground Color/lightGray))

    (. info-swap-button
      (addActionListener
	(proxy [ActionListener] []
	  (actionPerformed 
	   [evt]
	   (dosync (ref-set *TRANSPOSED* (not @*TRANSPOSED*)))

	   (. info-sample-text (setText (str (count (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*)))))
	   (. info-feature-text (setText (str (try (alength (first (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*))) (catch Exception e 0)))))

	   (if @*SNPMODE*
	     (do
	       (. plot-panel (setChart kmodes-combined-chart))
	       (. plot-panel (setRangeZoomable false))
	       ; remove old plots
	       (let [tmp (. kmodes-combined-plot (getSubplots))]
		 (dotimes [i (. tmp (size))]
		   (. kmodes-combined-plot (remove (. tmp (get 0))))))
	       ; add new plot
	       (. kmodes-combined-plot (add (create-kmodes-cluster-plot (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*)))))

	     (do (. plot-panel (setChart plot-area))
		 (. plot-panel (setRangeZoomable true))
		 ; remove old plots
		 (doall (map (fn [idx] (doto plot-data (. removeSeries (str "cluster " idx)))) (drop 1 (range (inc (. plot-data (getSeriesCount)))))))
		 ; add new plots
		 (doall (map (fn [idx x] (doto plot-data (. addSeries (str "cluster " idx) x))) (iterate inc 1) (data2plotdata (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) @*DIMX* @*DIMY*)))))

	   ; remove boxplots
	   (. boxplot-data (clear))))))

    (doto info-panel
      (. setLayout (FlowLayout. FlowLayout/RIGHT))
      (. add info-swap-button)
      (. add info-sample-label)
      (. add info-sample-text)
      (. add info-feature-label)
      (. add info-feature-text))

    (. file-chooser (setAcceptAllFileFilterUsed false))
    (. file-chooser
       (addChoosableFileFilter
	(proxy [FileFilter] []
	  (getDescription [] "TAB and SNP files")
	  (accept
	   [f]
	   (if (. f (isDirectory))
	     true
	     (let [fname (. f getName)
		   idx (inc (. fname (lastIndexOf ".")))
		   extension (. fname (substring idx))]
	       (if (or (. extension equalsIgnoreCase "tab")
		       (. extension equalsIgnoreCase "snp"))
		 true
		 false)))))))
    
    (.. boxplot-chart getCategoryPlot getRenderer (setMaximumBarWidth 0.25))
    (.. boxplot-chart getCategoryPlot getRenderer (setFillBox true))
    (.. boxplot-chart getCategoryPlot getRenderer (setMeanVisible false))
    (.. boxplot-chart getCategoryPlot getRangeAxis (setRange 0.0 1.05))
    (.. boxplot-chart getCategoryPlot getDomainAxis (setCategoryMargin 0.4))
    (.. boxplot-chart getCategoryPlot (setDomainGridlinePosition CategoryAnchor/MIDDLE))
    (.. boxplot-chart getCategoryPlot (setDomainGridlinesVisible true))

;    (.. kmodes-chart getCategoryPlot getRangeAxis (setRange -0.5 2.5))
;    (.. kmodes-chart getCategoryPlot getRangeAxis (setStandardTickUnits (NumberAxis/createIntegerTickUnits)))
;    (.. kmodes-chart getCategoryPlot (setDomainGridlinesVisible true))

;    (.. kmodes-chart getPlot getRangeAxis (setStandardTickUnits (NumberAxis/createIntegerTickUnits)))
;    (.. kmodes-chart getPlot getDomainAxis (setStandardTickUnits (NumberAxis/createIntegerTickUnits)))
;    (.. kmodes-chart getPlot getRangeAxis (setRange -0.5 2.5))
    (.. kmodes-combined-plot getDomainAxis (setStandardTickUnits (NumberAxis/createIntegerTickUnits)))
;    (.. kmodes-combined-plot getRangeAxis (setRange -0.5 2.5))
    (. kmodes-combined-chart (removeLegend))

    (. plot-panel (setPopupMenu nil))

    (. result-text (setLineWrap true))
    
    (. statusbar (setForeground (. Color red)))
;    (. run-button (setBackground (. Color red)))

    (. numcluster-text (setText (pr-str @*K*)))
    (. maxiter-text (setText (pr-str @*MAXITER*)))
    (. numcluster-kmodes-text (setText (pr-str @*K*)))
    (. maxiter-kmodes-text (setText (pr-str @*MAXITER*)))

;    (. dimx-text (setText "0"))
;    (. dimy-text (setText "1"))
    (. estimate-k-text (setText (pr-str @*CNEMAX*)))
    (. estimate-run-text (setText (pr-str @*ERUNS*)))
    (. estimate-maxiter-text (setText (pr-str @*MAXITER*)))

    (. run-button
       (addActionListener
	(proxy [ActionListener] []
	  (actionPerformed 
	   [evt]
	   (. statusbar (setText " running cluster analysis ... this may take some time ..."))
	   (dosync (ref-set *K* (try (. Integer (parseInt (. numcluster-text (getText)))) (catch Exception e @*K*))))
	   (. numcluster-text (setText (str @*K*)))
	   (dosync (ref-set *MAXITER* (try (. Integer (parseInt (. maxiter-text (getText)))) (catch Exception e @*MAXITER*))))
	   (. maxiter-text (setText (str @*MAXITER*)))
	   (let [res (kmeans (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) @*K* @*MAXITER* @*SNPMODE*)
		 old (. plot-data (getSeriesCount))]
	     (dosync (ref-set *RESULT* res))
	     (if @*SNPMODE*
	       (do
		 ; remove old plots
		 (let [tmp (. kmodes-combined-plot (getSubplots))]
		   (dotimes [i (. tmp (size))]
		     (. kmodes-combined-plot (remove (. tmp (get 0))))))

		 ; add new plots
		 (let [clusterres (vec (:cluster res))
		       data (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*)
		       len (count data)
		       datlist (loop [idx (int 0)
				      ret (vec (replicate @*K* '()))]
				 (if (< idx len)
				   (recur (inc idx) (assoc ret (clusterres idx) (cons (data idx) (ret (clusterres idx)))))
				   ret))]
		   (doall (map #(. kmodes-combined-plot (add (create-kmodes-cluster-plot %))) datlist))))
			       



	       (do (doall (map (fn [idx]
				 (doto plot-data (. removeSeries (str "cluster " idx)))) (drop 1 (range (inc old)))))
		   (doall (map (fn [idx x]
				 (doto plot-data (. addSeries (str "cluster " idx) x))) (iterate inc 1) (make-plotdata (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) @*DIMX* @*DIMY* @*RESULT*)))))

	     (. statusbar (setText " finished clustering")))))))

    (. estimate-button
       (addActionListener
	(proxy [ActionListener] []
	  (actionPerformed
	   [evt]
	   (. statusbar (setText " running cluster number estimation ... this will take some time ..."))
	   (dosync (ref-set *CNEMAX* (try (. Integer (parseInt (. estimate-k-text (getText)))) (catch Exception e 10))))
	   (. estimate-k-text (setText (pr-str @*CNEMAX*)))
	   (dosync (ref-set *ERUNS* (try (. Integer (parseInt (. estimate-run-text (getText)))) (catch Exception e 10))))
	   (. estimate-run-text (setText (pr-str @*ERUNS*)))
	   (dosync (ref-set *MAXITER* (try (. Integer (parseInt (. estimate-maxiter-text (getText)))) (catch Exception e @*MAXITER*))))
	   (. estimate-maxiter-text (setText (pr-str @*MAXITER*)))
	   
	   (let [ks (drop 2 (range (inc @*CNEMAX*)))
		 clusterresults (calculate-mca-results (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) @*ERUNS* ks @*MAXITER* @*SNPMODE*)
		 baselineresults (calculate-mca-baselines (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) @*ERUNS* ks @*SNPMODE*)
		 len (count clusterresults)
		 bestk (get-best-k clusterresults baselineresults)
		 res (kmeans (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) bestk @*MAXITER* @*SNPMODE*)
		 old (. plot-data (getSeriesCount))]
	     (. boxplot-data (clear))
	     (dorun (map #(.add boxplot-data %1 %2 %3) clusterresults (replicate len "McKmeans result") (iterate inc 2)))
	     (dorun (map #(.add boxplot-data %1 %2 %3) baselineresults (replicate len "Random prototype baseline") (iterate inc 2)))
	     (dosync (ref-set *K* bestk))
	     (dosync (ref-set *RESULT* res))
	     (. numcluster-text (setText (pr-str @*K*)))
	     (. statusbar (setText " finished cluster number estimation"))
	     
	     (if @*SNPMODE*
	       (do
		 ; remove old plots
		 (let [tmp (. kmodes-combined-plot (getSubplots))]
		   (dotimes [i (. tmp (size))]
		     (. kmodes-combined-plot (remove (. tmp (get 0))))))
		 ; add new plots
		 (let [clusterres (vec (:cluster res))
		       data (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*)
		       len (count data)
		       datlist (loop [idx (int 0)
				      ret (vec (replicate @*K* '()))]
				 (if (< idx len)
				   (recur (inc idx) (assoc ret (clusterres idx) (cons (data idx) (ret (clusterres idx)))))
				   ret))]
		   (doall (map #(. kmodes-combined-plot (add (create-kmodes-cluster-plot %))) datlist))))

	       (do
		 (doall (map (fn [idx] (doto plot-data (. removeSeries (str "cluster " idx)))) (drop 1 (range (inc old)))))
		 (doall (map (fn [idx x] (doto plot-data (. addSeries (str "cluster " idx) x))) (iterate inc 1) (make-plotdata (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) @*DIMX* @*DIMY* @*RESULT*))))))))))

    (doto cluster-panel
      (. setLayout (new FlowLayout FlowLayout/LEFT 35 5))
      (. add numcluster-label)
      (. add numcluster-text))
			
    (doto estimation-panel
      (. setLayout (new FlowLayout FlowLayout/LEFT))
      (. add estimate-k-label)
      (. add estimate-k-text))

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
	  (actionPerformed
	   [evt]
	   (try
	    (let [ret (. file-chooser (showOpenDialog frame))
		  filename (. (. file-chooser (getSelectedFile)) (getPath))
		  snp (. (. filename (substring (inc (. filename (lastIndexOf "."))))) (equalsIgnoreCase "snp"))
		  ;dataset (load-tab-file filename snp)
		  dataset (if-not snp (read-csv filename "\t" false csv-parse-double) (read-csv filename "\t" false csv-parse-int))
		  old (. plot-data (getSeriesCount))]
	      (dosync (ref-set *SNPMODE* snp))
	      (dosync (ref-set *DATASET* dataset))
	      (dosync (ref-set *TDATASET* (transpose dataset snp)))
	      (dosync (ref-set *TRANSPOSED* false))

	      (. info-sample-text (setText (str (count dataset))))
	      (. info-feature-text (setText (str (alength (first dataset)))))

	      (if @*SNPMODE*
		(do
		  ; remove old plots
		  (let [tmp (. kmodes-combined-plot (getSubplots))]
		    (dotimes [i (. tmp (size))]
		      (. kmodes-combined-plot (remove (. tmp (get 0))))))
	          ; add new plot
		  (. kmodes-combined-plot (add (create-kmodes-cluster-plot (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*))))
		  (. plot-panel (setChart kmodes-combined-chart))
		  (. plot-panel (setRangeZoomable false)))

;(JOptionPane/showMessageDialog nil (str ) "" JOptionPane/ERROR_MESSAGE)
		(do (. plot-panel (setChart plot-area))
		    (. plot-panel (setRangeZoomable true))
		    (doall (map (fn [idx] (doto plot-data (. removeSeries (str "cluster " idx)))) (drop 1 (range (inc old)))))
		    (doall (map (fn [idx x] (doto plot-data (. addSeries (str "cluster " idx) x))) (iterate inc 1) (data2plotdata (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) @*DIMX* @*DIMY*)))))

	      (. boxplot-data (clear)) 
	      (. result-text (setText ""))
	      (. statusbar
		 (setText " file loaded")))
	    (catch Exception e (JOptionPane/showMessageDialog nil "Error while loading file. See 'Help - File format'\nfor information about supported formats." "Error" JOptionPane/ERROR_MESSAGE)))))))

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
					(let [which-chart (. tabbed-pane (getSelectedIndex))
					      ret (. file-chooser (showSaveDialog frame))
					      filename (. (. file-chooser (getSelectedFile)) (getPath))]
					  (if (== which-chart 0)
					    (exportChart2SVG plot-area frame filename)
					    (exportChart2SVG boxplot-chart frame filename)))
					(catch Exception e nil))))))

		(. menu-preferences
		  (addActionListener
		   (proxy [ActionListener] []
			  (actionPerformed [evt]
					   (show-preferences-panel)))))
		
		(. menu-exit
		  (addActionListener
		   (proxy [ActionListener] []
			  (actionPerformed [evt]
					   (System/exit 0)))))		
				
		(doto menu-file
		  (. add menu-file-load)
		  (. add menu-file-save)
		  (. add menu-export-svg)
		  (. add menu-preferences)
		  (. add menu-exit))

;		(. menu-options-clusters
;		  (addActionListener
;		   (proxy [ActionListener] []
;			  (actionPerformed [evt]
;					   (show-options-panel)))))

;		(doto menu-options
;		  (. add menu-options-clusters))

		(. menu-help-about
		   (addActionListener
		    (proxy [ActionListener] []
			   (actionPerformed [evt]
					    (show-about-panel)))))
;					    (. statusbar (setText " about - not working yet"))))))
	
		(doto menu-help
		  (. add menu-help-about))
		
		(doto menubar
			(. add menu-file)
;			(. add menu-options)
			(. add menu-help))

;; 		(doto kmeans-options-panel
;; 		  (. setLayout (new GridLayout 4 2 5 5))
;; 		  (. add numcluster-label)
;; 		  (. add numcluster-text)
;; 		  (. add maxiter-label)
;; 		  (. add maxiter-text)
;; 		  (. add (JLabel. ""))
;; 		  (. add (JLabel. ""))
;; 		  (. add (JLabel. ""))
;; 		  (. add run-button))
		
		(let [layout (GroupLayout. kmeans-options-panel)
		      parGrouplabelh (. layout (createParallelGroup))
		      parGrouptexth (. layout (createParallelGroup))
		      parGrouplabelv (. layout (createParallelGroup))
		      parGrouptextv (. layout (createParallelGroup))
		      seqGrouph (. layout (createSequentialGroup))
		      seqGroupv (. layout (createSequentialGroup))]
		  (. layout setAutoCreateGaps true)
		  (. layout setAutoCreateContainerGaps true)
		  (doto parGrouplabelh
		    (. addComponent numcluster-label)
		    (. addComponent maxiter-label))
		  (doto parGrouptexth
		    (. addComponent numcluster-text
		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
		    (. addComponent maxiter-text 
		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
		    (. addComponent run-button
		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE))
		  (doto seqGrouph
		    (. addGroup parGrouplabelh)
		    (. addGroup parGrouptexth))
		  (. layout setHorizontalGroup seqGrouph)		  
		  (doto parGrouplabelv
		    (. addComponent numcluster-label)
		    (. addComponent numcluster-text))
		  (doto parGrouptextv
		    (. addComponent maxiter-label)
		    (. addComponent maxiter-text))
		  (doto seqGroupv
		    (. addGroup parGrouplabelv)
		    (. addGroup parGrouptextv)
		    (. addComponent run-button))
		  (. layout setVerticalGroup seqGroupv)
		  (. kmeans-options-panel setLayout layout))

		(let [layout (GroupLayout. cne-options-panel)
		      parGrouplabelh (. layout (createParallelGroup))
		      parGrouptexth (. layout (createParallelGroup))
		      parGrouplabelv (. layout (createParallelGroup))
		      parGrouptextv (. layout (createParallelGroup))
		      parGrouprunv (. layout (createParallelGroup))
		      seqGrouph (. layout (createSequentialGroup))
		      seqGroupv (. layout (createSequentialGroup))]
		  (. layout setAutoCreateGaps true)
		  (. layout setAutoCreateContainerGaps true)
		  (doto parGrouplabelh
		    (. addComponent estimate-k-label)
		    (. addComponent estimate-maxiter-label)
		    (. addComponent estimate-run-label))
		  (doto parGrouptexth
		    (. addComponent estimate-k-text
		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
		    (. addComponent estimate-maxiter-text 
		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
		    (. addComponent estimate-run-text
		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
		    (. addComponent estimate-button
		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE))
		  (doto seqGrouph
		    (. addGroup parGrouplabelh)
		    (. addGroup parGrouptexth))
		  (. layout setHorizontalGroup seqGrouph)		  
		  (doto parGrouplabelv
		    (. addComponent estimate-k-label)
		    (. addComponent estimate-k-text))
		  (doto parGrouptextv
		    (. addComponent estimate-maxiter-label)
		    (. addComponent estimate-maxiter-text))
		  (doto parGrouprunv
		    (. addComponent estimate-run-label)
		    (. addComponent estimate-run-text))
		  (doto seqGroupv
		    (. addGroup parGrouplabelv)
		    (. addGroup parGrouptextv)
		    (. addGroup parGrouprunv)
		    (. addComponent estimate-button))
		  (. layout setVerticalGroup seqGroupv)
		  (. cne-options-panel setLayout layout))

;; 		(doto cne-options-panel
;; 		  (. setLayout (new GridLayout 4 2 5 5))
;; 		  (. add estimate-k-label)
;; 		  (. add estimate-k-text)
;; 		  (. add estimate-maxiter-label)
;; 		  (. add estimate-maxiter-text)
;; 		  (. add estimate-run-label)
;; 		  (. add estimate-run-text)
;; 		  (. add (JLabel. ""))
;; 		  (. add estimate-button))

		(doto kmeans-panel
		  (. setLayout (new BorderLayout))
		  (. add plot-panel BorderLayout/CENTER)
		  (. add kmeans-options-panel BorderLayout/SOUTH))

;		(doto kmodes-panel
;		  (. setLayout (new BorderLayout))
;		  (. add kmodes-plot-panel BorderLayout/NORTH)
;		  (. add kmodes-options-panel BorderLayout/CENTER))

		(doto cne-panel
		  (. setLayout (new BorderLayout))
		  (. add boxplot-panel BorderLayout/CENTER)
		  (. add cne-options-panel BorderLayout/SOUTH))

		(doto tabbed-pane
		  (. addTab "Cluster analysis" nil kmeans-panel "Perform cluster analysis")
;		  (. addTab "Cluster analysis" nil kmodes-panel "Perform cluster analysis")
		  ;(. addTab "K-modes" kmodes-panel)
		  (. addTab "Cluster number estimation" nil cne-panel "Perform cluster number estimation"))

		(doto frame
		  (. setSize 1280 1024)
		  (. setJMenuBar menubar)
		  (. setLayout (new BorderLayout))
		  (. add tabbed-pane (. BorderLayout CENTER))
		  (. add info-panel (. BorderLayout SOUTH))
;			(. add result-panel (. BorderLayout SOUTH))
		  (. pack)
		  (. setDefaultCloseOperation (. JFrame EXIT_ON_CLOSE))
		  (. setVisible true))))


(defn runKmeans [outfile]
  (let [res (kmeans @*DATASET* @*K* @*MAXITER* @*SNPMODE*)
	restext (.. (pr-str (:cluster res)) (replace "(" "") (replace ")" ""))]
    (save-result outfile restext)))

(defn runCNE [outfile cneoutfile]
  (let [ks (drop 2 (range (inc @*CNEMAX*)))
	clusterresults (calculate-mca-results @*DATASET* @*ERUNS* ks @*MAXITER* @*SNPMODE*)
	baselineresults (calculate-mca-baselines @*DATASET* @*ERUNS* ks @*SNPMODE*)
	bestk (get-best-k clusterresults baselineresults)
	res (kmeans @*DATASET* bestk @*MAXITER* @*SNPMODE*)
	restext (.. (pr-str (:cluster res)) (replace "(" "") (replace ")" ""))]
    (save-clusternumber-result clusterresults baselineresults cneoutfile)
    (save-result outfile restext)))

(defn runCMD [infile outfile k maxiter cne? cnemax cneruns cneoutfile]
  (dosync (ref-set *SNPMODE* (. (. infile (substring (inc (. infile (lastIndexOf "."))))) (equalsIgnoreCase "snp"))))
; (dosync (ref-set *DATASET* (load-tab-file infile @*SNPMODE*)))
  (dosync (ref-set *DATASET* (if-not @*SNPMODE* (read-csv infile "\t" false csv-parse-double) (read-csv infile "\t" false csv-parse-int))))
  (if (string? k)
    (dosync (ref-set *K* (. Integer (parseInt k))))
    (dosync (ref-set *K* k)))
  (if (string? maxiter)
    (dosync (ref-set *MAXITER* (. Integer (parseInt maxiter))))
    (dosync (ref-set *MAXITER* maxiter)))
  (if (string? cneruns)
    (dosync (ref-set *ERUNS* (. Integer (parseInt cneruns))))
    (dosync (ref-set *ERUNS* cneruns)))
  (if (string? cnemax)
    (dosync (ref-set *CNEMAX* (. Integer (parseInt cnemax))))
    (dosync (ref-set *CNEMAX* cnemax)))
  (if cne?
    (do (println "Starting McKmeans cluster number estimation."); Find the output in " outfile " and " cneoutfile)
	(runCNE outfile cneoutfile))
    (do (println "Starting McKmeans cluster analysis."); Find the output in " outfile)
	(runKmeans outfile)))
  (System/exit 0))

(defn -main [& args] 
  (if (nil? args)
    (SwingUtilities/invokeLater runGUI)
    (with-command-line args
      "Command line usage"
      [[infile i "The name of the input file."]
       [outfile o "The name of the output file." "clustering.txt"]
       [k "The number of clusters" 2]
       [maxiter "The maximum number of iterations allowed." 10]
       [cne? "Run a cluster number estimation? This is a boolean flag."]
       [cnemax "The maximal number of clusters for the cluster number estimation." 10]
       [cneruns "The number of repeated runs of clusterings for each partitioning." 10]
       [cneoutfile co "The name of the output file for the cluster number estimation." "cne.txt"]]
      (if (nil? infile)
	(println "Please provide a valid input file.")
	(runCMD infile outfile k maxiter cne? cnemax cneruns cneoutfile)))))
