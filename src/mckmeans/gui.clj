
;;;;
; File  : GUI for McKmeans application
; Author: Johann Kraus
;;;;

(ns mckmeans.gui
  #^{:author "Johann M. Kraus",
     :doc "Multi-core kmeans cluster application"}
  (:use (mckmeans kmeans utils cne)
	clojure.contrib.command-line)
  (:import (javax.swing JFrame JLabel JButton JPanel JMenuBar JMenu JMenuItem JFileChooser JTextField JCheckBox JTextArea JScrollPane JTabbedPane JOptionPane SwingUtilities GroupLayout JEditorPane JList DefaultListModel JProgressBar)
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
	   (org.w3c.dom DOMImplementation Document)
	   (javastat.inference.nonparametric RankSumTest))
  (:gen-class))

;;;; GLOBALS ;;;;
(def *TRANSPOSED* (ref false))
(def *TDATASET* (ref nil))
(def *DATASET* (ref nil))
(def *RESULT* (ref nil))
(def *K* (ref 2))
(def *MAXITER* (ref 10))
(def *ERUNS* (ref 10))
(def *CNEMIN* (ref 2))
(def *CNEMAX* (ref 10))
(def *DIMX* (ref 0))
(def *DIMY* (ref 1))
(def *SNPMODE* (ref false))
(def *BESTKRUNS* (ref 10))
(def *NSTARTS* (ref 1))
(def *NSTARTSCNE* (ref 10))

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
  (let [data (doall (map (fn [x] (make-array (. Double TYPE) 2 (count (filter #(= x %) ass)))) (range @*K*)))
	ass (vec ass)
	maxidx (count ass)]
    (loop [idx 0 idxs (replicate @*K* 0)]
      (if (< idx maxidx)
	(let [curidx (nth ass idx)
	      curdata (nth data curidx)
	      curdataidx (nth idxs curidx)]
	  (aset-double (aget curdata 0) curdataidx (nth (nth dataset idx) dimx))
	  (aset-double (aget curdata 1) curdataidx (nth (nth dataset idx) dimy))
	  (recur (inc idx) (doall (map (fn [x y] (if (= curidx y) (inc x) x)) idxs (iterate inc 0)))))
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
;;     (let [rend (. cluster-plot getRenderer)]
;;       (map #(. rend (setSeriesPaint % Color/BLACK)) (range (. cluster-plot getSeriesCount))))

    cluster-plot))

;; plot-sammon
;; (defn plot-sammon [dat]
;;   (let [plot-data (DefaultXYDataset.)
;; 	plot-area (. ChartFactory (createScatterPlot "" "x" "y" plot-data (. PlotOrientation VERTICAL) true false false))
;; 	plot-panel (ChartPanel. plot-area)
;; 	plot-frame (JFrame.)]

;;     (doall (map (fn [idx x] (doto plot-data (. addSeries (str idx) x))) (iterate inc 1) (data2plotdata dat 0 1)))

;;     (doto plot-frame
;;       (. setSize 400 400)
;;       (. setLayout (new BorderLayout))
;;       (. add plot-panel (. BorderLayout CENTER))
;;       (. pack)
;;       (. setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
;;       (. setVisible true))))

;; about panel
(defn show-about-panel
  []
  (let [about-panel (JPanel.)
	about-frame (JFrame. "About")
	about-text (JTextArea. 5 1)
	close-button (JButton. "Close")]
    (. about-text (setEditable false))
    (. about-text (setText "\tMcKmeans\n\nMulti-core K-means cluster analysis.\n\nVersion:\t0.42\nAuthors:\tJohann M. Kraus\n\tHans A. Kestler\nCopyright:\tArtistic Licence 2.0"))
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
  (let [help-index "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\"
               \"http://www.w3.org/TR/html4/loose.dtd\">
               <html>
               <head>
               <title>McKmeans Help</title>
               </head>
               <body>
               <h1>McKmeans: A highly efficient multi-core k-means algorithm<br>for clustering extremely large datasets.</h1>
               <ul>
               <li><a href=\"help-usage\">Basic usage</a></li>
               <li><a href=\"help-load\">Input file format</a></li>
               <li><a href=\"help-sammon\">Sammon's projection</a></li>
               <li><a href=\"help-online\">Online help</a></li>
               </ul>
               </body>
               </html>"
	help-usage "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\"
               \"http://www.w3.org/TR/html4/loose.dtd\">
               <html>
               <head>
               <title>McKmeans Help</title>
               </head>
               <body>
               <h1>Basic usage</h1>
               <h2>Load data, transpose data</h2>
               First load a gene expression or SNP data set via 'File -> Load'. The required input file format is described <a href=\"help-load\">here</a>. Information about the number of rows and colums is displayed on the bottom right. The data is always clustered row-wise. The input data can be transposed to switch rows and columns by pressing the 'Transpose data set!' button. In case of gene expression data, the first two columns of the data set are displayed as a scatterplot in the plotting region. The columns to plot can be changed in the text field of 'Column to plot vertically/horizontally'. Alternatively, the data can be plotted as a 2-dimensional non-linear projection, see <a href=\"help-sammon\">Sammon's projection</a> for details. SNP data is displayed as a parallel coordinate plot. To zoom in select a region with the mouse.
               <h2>Cluster analysis</h2>
Choose the number of clusters and the maximal number of iterations for the K-means algorithm. Optionally, choose the number of restarts for K-means. As K-means in initialized randomly, different runs of clustering can give different results. If more than one run of K-means is performed, the clustering with the minimal within-cluster sum of squares is given as result. Clustering of SNP data uses a slightly different method for calculating distances, see <a href=\"online-help\">the online help</a> for more details. The clustering starts by clicking on the 'Cluster!' button. The cluster analysis may take a while, e.g. 25 minutes for clustering a data set containing 1000000 rows with 100 columns into 20 clusters on a dual-quad core computer. The resulting clustering is displayed in the plotting area. Additionally, the assignment vector can be saved via 'File -> Save'.
               <h2>Cluster number estimation</h2>
               Switch to the cluster number estimation panel by choosing the second tab in the main window. Choose the minimal number of clusters, maximal number of clusters, the maximal number of iterations per clustering, the maximal number of runs per clustering, and the maximal runs of repetitions of kmeans per clustering. Press 'Cluster number estimation' to start the process. Depending on data size, this may take several hours. The result is displayed in the boxplot area. Red boxes show the MCA-index from evaluating cluster results. Blue boxes show a random baseline for the MCA-index. The best number of clusters is reported for the number k with the largest difference between mean MCA-index from clustering and mean MCA-index from baseline evalution. The best clustering is computed by running 10 repeated clusterings (can be changed in the 'Preferences' dialog) for the best number of clusters k. The best solution (minimal within-cluster sum of squares) is displayed on the cluster analysis tab.
               </body>
               </html>"
	help-load "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\"
               \"http://www.w3.org/TR/html4/loose.dtd\">
               <html>
               <head>
               <title>McKmeans help</title>
               </head>
               <body>
               <h1>Input file format</h1>
               Input is required to be in CSV file format. The data is clustered by rows, but it can be transposed after loading for also clustering columns. Columns are separated by ','. Lines starting with the character '#' (e.g. header line) are omitted. SNP data has to be encoded as '0,1,2' for 'homozygotous reference, heterozygotous, homozygotous alternative'. For clustering snp files, the suffix has to be changed to '.snp'.
               </body>
               </html>"
	help-sammon "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\"
               \"http://www.w3.org/TR/html4/loose.dtd\">
               <html>
               <head>
               <title>McKmeans help</title>
               </head>
               <body>
               <h1>Sammon's projection</h1>
               Sammon's projection method [1] is a non-linear projection method to map high-dimensional data to lower dimensionality. Our implementation uses a heuristic proposed by Kohonen [2] to decrease runtime. However, for large data sets (> 500 rows) the computation may take several minutes.<br><br>[1] J. W. Sammon, Jr. 'A nonlinear mapping for data structure analysis'. IEEE Transactions on Computers, 18, pp. 401–409, 1969.<br>[2] T. Kohonen. Self Organizing Maps. Springer, 3 edition, 2001
               </body>
               </html>"
	editor-panel (JEditorPane. "text/html" help-index)
	scroll-panel (JScrollPane. editor-panel)
	button-panel (JPanel.)
	help-frame (JFrame. "Help")
	close-button (JButton. "Close")
	contents-button (JButton. "Contents")]
    (. editor-panel setEditable false)
    (. editor-panel 
       (addHyperlinkListener
	(proxy [HyperlinkListener] []
	  (hyperlinkUpdate
	   [evt]
	   (if (= (. evt getEventType) (. javax.swing.event.HyperlinkEvent$EventType ACTIVATED))
	     (let [description (. evt (getDescription))]
	       (cond (= description "help-load") (. editor-panel (setText help-load))
		     (= description "help-usage") (. editor-panel (setText help-usage))
		     (= description "help-sammon") (. editor-panel (setText help-sammon))
		     (= description "help-online") (.. java.awt.Desktop getDesktop (browse (java.net.URI. "http://www.informatik.uni-ulm.de/ni/staff/HKestler/parallelkmeans"))))))))))
;		     (= description "help-online") (. editor-panel (setPage "http://www.informatik.uni-ulm.de/ni/staff/HKestler/parallelkmeans"))
;		     :default (.. java.awt.Desktop getDesktop (browse (java.net.URI. description))))))))))

    (doto button-panel
      (. setLayout (FlowLayout. FlowLayout/RIGHT))
      (. add contents-button)
      (. add close-button))
    (. close-button
      (addActionListener
       (proxy [ActionListener] []
	 (actionPerformed
	  [evt]
	  (. help-frame (dispose))))))
    (. contents-button
      (addActionListener
       (proxy [ActionListener] []
	 (actionPerformed
	  [evt]
	  (. editor-panel (setText help-index))))))
    (doto help-frame
      (. setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (. setSize 1000 1000)
      (. setLayout (new BorderLayout))
      (. add scroll-panel BorderLayout/CENTER)
      (. add button-panel BorderLayout/SOUTH)
      (. pack)
      (. setVisible true))))

;; options panel
(defn show-preferences-panel
  []
  (let [options-panel (JPanel.)
	options-frame (JFrame. "Preferences")
;; 	dim-label (new JLabel " Select columns to plot:")
;; 	dimx-label (new JLabel " dim x:")
;; 	dimy-label (new JLabel " dim y:")
;; 	dimx-text (new JTextField)
;; 	dimy-text (new JTextField)
	bestkruns-label (JLabel. "Best clustering nstarts:")
	bestkruns-text (JTextField. )
	save-estimation (new JCheckBox "" false)
	update-button (new JButton "Update")]

    (. update-button
       (addActionListener
        (proxy [ActionListener] []
	  (actionPerformed [evt]
			   (let [;; dimx (try (. Integer (parseInt (. dimx-text (getText)))) (catch Exception e @*DIMX*))
				 ;; dimy (try (. Integer (parseInt (. dimy-text (getText)))) (catch Exception e @*DIMY*))
				 saveres (not (nil? (. save-estimation (getSelectedObjects))))
				 bestkruns (try (. Integer (parseInt (. bestkruns-text (getText)))) (catch Exception e @*BESTKRUNS*))]
;; 			     (dosync (ref-set *DIMX* dimx))
;; 			     (dosync (ref-set *DIMY* dimy))
			     (dosync (ref-set *SAVERES* saveres))
			     (dosync (ref-set *BESTKRUNS* bestkruns)))
			   (. options-frame (dispose))))))

;;     (. dimx-text (setText (pr-str @*DIMX*)))
;;     (. dimy-text (setText (pr-str @*DIMY*)))
    (. save-estimation (setSelected @*SAVERES*))
    (. bestkruns-text (setText (str @*BESTKRUNS*)))
    
    (doto options-panel
      (. setLayout (new GridLayout 2 3 5 5))
      
;;       (. add dim-label)
;;       (. add dimx-label)
;;       (. add dimy-label)

;;       (. add (new JLabel ""))
;;       (. add dimx-text)
;;       (. add dimy-text)

      (. add bestkruns-label)
      (. add bestkruns-text)

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


;; (defn show-options-panel
;;   ""
;;   []
;;   (let [options-panel (JPanel.)
;; 	options-frame (JFrame. "Options")
;; 	dim-label (new JLabel " Select columns to plot:")
;; 	dimx-label (new JLabel " dim x:")
;; 	dimy-label (new JLabel " dim y:")
;; 	dimx-text (new JTextField)
;; 	dimy-text (new JTextField)
;; 	maxiter-text (new JTextField)
;; 	maxiter-label (new JLabel "Maximal number of iterations:")
;; 	estimate-label (new JLabel "Cluster number estimation parameters:")
;; 	estimate-run-label (new JLabel "Number of resamplings per cluster:")
;; 	estimate-run-text (new JTextField)
;; 	parameter-label (new JLabel "K-means parameters:")
;; 	save-estimation (new JCheckBox "Save cluster results" false)
;; 	update-button (new JButton "Update")]

;;     (. update-button
;;        (addActionListener
;;         (proxy [ActionListener] []
;; 	  (actionPerformed [evt]
;; 			   (let [eruns (try (. Integer (parseInt (. estimate-run-text (getText)))) (catch Exception e @*ERUNS*))
;; 				 maxiter (try (. Integer (parseInt (. maxiter-text (getText)))) (catch Exception e @*MAXITER*))
;; 				 dimx (try (. Integer (parseInt (. dimx-text (getText)))) (catch Exception e @*DIMX*))
;; 				 dimy (try (. Integer (parseInt (. dimy-text (getText)))) (catch Exception e @*DIMY*))
;; 				 saveres (not (nil? (. save-estimation (getSelectedObjects))))]
;; 			     (dosync (ref-set *DIMX* dimx))
;; 			     (dosync (ref-set *DIMY* dimy))
;; 			     (dosync (ref-set *MAXITER* maxiter))
;; 			     (dosync (ref-set *ERUNS* eruns))
;; 			     (dosync (ref-set *SAVERES* saveres)))
;; 			   (. options-frame (dispose))))))

;;     (. dimx-text (setText (pr-str @*DIMX*)))
;;     (. dimy-text (setText (pr-str @*DIMY*)))
;;     (. maxiter-text (setText (pr-str @*MAXITER*)))
;;     (. estimate-run-text (setText (pr-str @*ERUNS*)))
;;     (. save-estimation (setSelected @*SAVERES*))
    
;;     (doto options-panel
;;       (. setLayout (new GridLayout 7 3 5 5))
      
;;       (. add dim-label)
;;       (. add dimx-label)
;;       (. add dimy-label)
;;                         ;(. add (new JLabel ""))
;;       (. add (new JLabel ""))
;;       (. add dimx-text)
;;       (. add dimy-text)
;;                         ;(. add plot-button)

;;       (. add parameter-label)
;;                         ;(. add numcluster-label)
;;       (. add maxiter-label)
;;       (. add (new JLabel ""))
;;       (. add (new JLabel ""))
;;                         ;(. add numcluster-text)
;;       (. add maxiter-text)
;;       (. add (new JLabel ""))
;;                         ;(. add run-button)

;;       (. add estimate-label)
;;                                         ;(. add estimate-k-label)
;;       (. add estimate-run-label)
;;       (. add (new JLabel ""))
;;       (. add (new JLabel ""))
;;                         ;(. add estimate-k-text)
;;       (. add estimate-run-text)
;;                         ;(. add estimate-button))
;;       (. add (new JLabel ""))
;;       (. add save-estimation)
;;       (. add (new JLabel ""))
;;       (. add update-button))

;;     (doto options-frame
;;       (. setSize 400 400)
;;       (. setLayout (new BorderLayout))
;;       (. add options-panel)
;;       (. pack)
;;       (. setVisible true))))

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

(defn boxplot-clusternumber [results baselines bestk filename]
  (let [dat (DefaultBoxAndWhiskerCategoryDataset.)
	frame (JFrame. "Cluster number estimation")
	chart (org.jfree.chart.ChartFactory/createBoxAndWhiskerChart "" "Cluster number" "MCA-index" dat true)
	plot-panel (new ChartPanel chart)
	len (count results)]

    (.. chart getCategoryPlot getRenderer (setMaximumBarWidth 0.25))
    (.. chart getCategoryPlot getRenderer (setFillBox true))
    (.. chart getCategoryPlot getRenderer (setMeanVisible false))
    (.. chart getCategoryPlot getRangeAxis (setRange 0.0 1.05))
    (.. chart getCategoryPlot getDomainAxis (setCategoryMargin 0.4))
    (.. chart getCategoryPlot (setDomainGridlinePosition CategoryAnchor/MIDDLE))
    (.. chart getCategoryPlot (setDomainGridlinesVisible true))
    
    (dorun (map #(.add dat %1 %2 (if (= %3 bestk) (str %3 "*") %3)) results (replicate len "McKmeans") (iterate inc 2)))
    (dorun (map #(.add dat %1 %2 (if (= %3 bestk) (str %3 "*") %3)) baselines (replicate len "Random prototype baseline") (iterate inc 2)))

;    (dorun (map #(.add dat %1 %2 %3) results (replicate len "McKmeans") (iterate inc 2)))
;    (dorun (map #(.add dat %1 %2 %3) baselines (replicate len "Random prototype baseline") (iterate inc 2)))

    (doto frame
      (. setSize 400 400)
      (. setLayout (new BorderLayout))
      (. add plot-panel)
      (. pack))

    (exportChart2SVG chart frame filename)))

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
	menu-help-help (new JMenuItem "Help")
	tabbed-pane (JTabbedPane.)

	kmeans-panel (JPanel.)
	kmodes-panel (JPanel.)
	cne-panel(JPanel.)

	kmeans-options-panel (JPanel.)
	kmodes-options-panel (JPanel.)
	cne-options-panel (JPanel.)

	plot-data (new DefaultXYDataset)
	plot-area (. ChartFactory (createScatterPlot "" "column 1" "column 2" plot-data (. PlotOrientation VERTICAL) true false false))
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
	nstart-label (JLabel. " Number of k-means restarts:")
	nstart-text (JTextField. 7)
	nstartcne-label (JLabel. "Number of k-means restarts:")
	nstartcne-text (JTextField. 7)

	result-label (new JLabel " Number of elements per cluster:")
;	result-list (DefaultListModel.)
;	result-text (JList. result-list)
	result-text (JTextArea. 2 15)
	result-scrollpane (JScrollPane. result-text)
	result-panel (new JPanel)

;	estimate-label (new JLabel " Change cluster number estimation parameters:")
	estimate-k-label (JLabel. " Maximal number of clusters k:")
	estimate-kmin-label (JLabel. " Minimal number of clusters k:")
	estimate-run-label (new JLabel " Number of resamplings per cluster:")
	estimate-k-text (new JTextField 7)
	estimate-kmin-text (new JTextField 7)
	estimate-run-text (new JTextField 7)
	estimate-button (new JButton "Run cluster number estimation!")
	estimate-maxiter-label (JLabel. " Maximal number of iterations:")
	estimate-maxiter-text (JTextField. 7)

        estimate-result-text (JTextField. 7)
	estimate-result-label (JLabel. "P-value best clustering:")
	estimate-result-k (JTextField. 7)
	estimate-result-klabel (JLabel. "Best k clustering:")

	statusbar (new JLabel " Welcome to the McKmeans cluster application ...")
	file-chooser (JFileChooser.)
	file-chooser-csv (JFileChooser.)
	file-chooser-svg (JFileChooser.)
	
	info-panel (JPanel.)
	info-sample-text (JTextField. 7)
	info-feature-text (JTextField. 7)
	info-sample-label (JLabel. "Number of rows:")
	info-feature-label (JLabel. "Number of columns:")
	info-swap-button (JButton. "Transpose data!")

	sammon-button (JButton. "Sammon mapping!")

	dim-label (new JLabel " Select columns to plot:")
	dimx-label (new JLabel " Column to plot horizontally: ")
	dimy-label (new JLabel " Column to plot vertically: ")
	dimx-text (new JTextField 7)
	dimy-text (new JTextField 7)
	dim-button (JButton. "Update columns to plot!")]

    (. sammon-button
       (addActionListener
	(proxy [ActionListener] []
	  (actionPerformed
	   [evt]
	   (. tabbed-pane (setSelectedIndex 0))

(let [job-frame (JFrame. "Job running")
      job-text (JTextField. "Running Sammon mapping...")
      job-runner (JProgressBar.)
      job-cancel (JButton. "Cancel")
      job-panel (JPanel.)]
  (. job-text (setEditable false))
  (. job-runner (setIndeterminate true))
  (. job-cancel
    (addActionListener
     (proxy [ActionListener] []
       (actionPerformed 
	[evt]
	(. job-frame (dispose))))))
  (doto job-panel
    (. setLayout (FlowLayout.))
    (. add job-runner)
    (. add job-cancel))
  (doto job-frame
    (. setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
;    (. setSize 10 100)
    (. setLayout (BorderLayout.))
    (. add job-text BorderLayout/NORTH)
    (. add job-panel BorderLayout/CENTER)
    (. pack)
    (. setVisible true))

(. sammon-button (setEnabled false))
(. (java.util.concurrent.Executors/newCachedThreadPool) (execute (fn [] 

	   (let [res (sammon (if @*TRANSPOSED* @*TDATASET* @*DATASET*) 10 2)]
	     ; remove old plots
	     (doall (map (fn [idx] (doto plot-data (. removeSeries (str "cluster " idx)))) (drop 1 (range (inc (. plot-data (getSeriesCount)))))))
	     ; add new plots
	     (doall (map (fn [idx x] (doto plot-data (. addSeries (str "cluster " idx) x))) (iterate inc 1) (data2plotdata res 0 1)))
	     ;(. result-text (setText (reduce #(str (if-not (= nil %1) (str %1 "\n")) (str "Cluster " (inc %2) ": " (count (filter (fn [a] (= %2 a)) (:cluster @*RESULT*))))) nil (range @*K*))))
	     (. result-text (setText (str (count (if @*TRANSPOSED* @*TDATASET* @*DATASET*)))))
	     )

(. sammon-button (setEnabled true))
(. job-frame (dispose))))))
))))

    (. info-sample-text (setBackground Color/lightGray))
    (. info-feature-text (setBackground Color/lightGray))
    (. info-swap-button
      (addActionListener
	(proxy [ActionListener] []
	  (actionPerformed 
	   [evt]
           (dosync (ref-set *RESULT* nil))
           (dosync (ref-set *DIMX* 0))
           (dosync (ref-set *DIMY* 1))
           (. dimx-text (setText (str (inc @*DIMX*))))
           (. dimy-text (setText (str (inc @*DIMY*))))
           ;set axis label
           (.. plot-area getXYPlot getDomainAxis (setLabel (str "column " (inc @*DIMX*))))
           (.. plot-area getXYPlot getRangeAxis (setLabel (str "column " (inc @*DIMY*))))

	   (dosync (ref-set *TRANSPOSED* (not @*TRANSPOSED*)))
	   (. result-text (setText (str "Cluster 1: " (count (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*)))))
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
      (. add sammon-button)
      (. add info-swap-button)
      (. add info-sample-label)
      (. add info-sample-text)
      (. add info-feature-label)
      (. add info-feature-text))

    (. file-chooser (setAcceptAllFileFilterUsed false))
    (. file-chooser
       (addChoosableFileFilter
	(proxy [FileFilter] []
	  (getDescription [] "CSV and SNP files")
	  (accept
	   [f]
	   (if (. f (isDirectory))
	     true
	     (let [fname (. f getName)
		   idx (inc (. fname (lastIndexOf ".")))
		   extension (. fname (substring idx))]
	       (if (or (. extension equalsIgnoreCase "csv")
		       (. extension equalsIgnoreCase "snp"))
		 true
		 false)))))))
 
    (. file-chooser-csv (setAcceptAllFileFilterUsed false))
    (. file-chooser-csv
       (addChoosableFileFilter
	(proxy [FileFilter] []
	  (getDescription [] "CSV files")
	  (accept
	   [f]
	   (if (. f (isDirectory))
	     true
	     (let [fname (. f getName)
		   idx (inc (. fname (lastIndexOf ".")))
		   extension (. fname (substring idx))]
	       (if (. extension equalsIgnoreCase "csv")
		 true
		 false)))))))
 
    (. file-chooser-svg (setAcceptAllFileFilterUsed false))
    (. file-chooser-svg
       (addChoosableFileFilter
	(proxy [FileFilter] []
	  (getDescription [] "SVG files")
	  (accept
	   [f]
	   (if (. f (isDirectory))
	     true
	     (let [fname (. f getName)
		   idx (inc (. fname (lastIndexOf ".")))
		   extension (. fname (substring idx))]
	       (if (. extension equalsIgnoreCase "svg")
		 true
		 false)))))))
 
    (.. boxplot-chart getCategoryPlot getRenderer (setMaximumBarWidth 0.25))
    (.. boxplot-chart getCategoryPlot getRenderer (setFillBox true))
    (.. boxplot-chart getCategoryPlot getRenderer (setMeanVisible false))
    (.. boxplot-chart getCategoryPlot getRangeAxis (setRange 0.0 1.05))
    (.. boxplot-chart getCategoryPlot getDomainAxis (setCategoryMargin 0.4))
    (.. boxplot-chart getCategoryPlot (setDomainGridlinePosition CategoryAnchor/MIDDLE))
    (.. boxplot-chart getCategoryPlot (setDomainGridlinesVisible true))

    (.. kmodes-combined-plot getDomainAxis (setStandardTickUnits (NumberAxis/createIntegerTickUnits)))

    (. kmodes-combined-chart (removeLegend))

    (. plot-panel (setPopupMenu nil))
    
    (. statusbar (setForeground (. Color red)))
;    (. run-button (setBackground (. Color red)))

    (. numcluster-text (setText (pr-str @*K*)))
    (. maxiter-text (setText (pr-str @*MAXITER*)))
    (. nstart-text (setText (str @*NSTARTS*)))
    (. nstartcne-text (setText (str @*NSTARTSCNE*)))
    (. numcluster-kmodes-text (setText (pr-str @*K*)))
    (. maxiter-kmodes-text (setText (pr-str @*MAXITER*)))

    (. estimate-k-text (setText (pr-str @*CNEMAX*)))
    (. estimate-kmin-text (setText (pr-str @*CNEMIN*)))
    (. estimate-run-text (setText (pr-str @*ERUNS*)))
    (. estimate-maxiter-text (setText (pr-str @*MAXITER*)))

    (. run-button
       (addActionListener
	(proxy [ActionListener] []
	  (actionPerformed 
	   [evt]

(let [job-frame (JFrame. "Job running")
      job-text (JTextField. "Running cluster analysis..")
      job-runner (JProgressBar.)
      job-cancel (JButton. "Cancel")
      job-panel (JPanel.)]
  (. job-text (setEditable false))
  (. job-runner (setIndeterminate true))
  (. job-cancel
    (addActionListener
     (proxy [ActionListener] []
       (actionPerformed 
	[evt]
	(. job-frame (dispose))))))
  (doto job-panel
    (. setLayout (FlowLayout.))
    (. add job-runner)
    (. add job-cancel))
  (doto job-frame
    (. setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
;    (. setSize 10 100)
    (. setLayout (BorderLayout.))
    (. add job-text BorderLayout/NORTH)
    (. add job-panel BorderLayout/CENTER)
    (. pack)
    (. setVisible true))

(. run-button (setEnabled false))
(. (java.util.concurrent.Executors/newCachedThreadPool) (execute (fn [] 

	   (. statusbar (setText "Running cluster analysis..."))
	   (dosync (ref-set *K* (try (. Integer (parseInt (. numcluster-text (getText)))) (catch Exception e @*K*))))
	   (. numcluster-text (setText (str @*K*)))
	   (dosync (ref-set *MAXITER* (try (. Integer (parseInt (. maxiter-text (getText)))) (catch Exception e @*MAXITER*))))
	   (. maxiter-text (setText (str @*MAXITER*)))
	   (dosync (ref-set *NSTARTS* (try (. Integer (parseInt (. nstart-text (getText)))) (catch Exception e @*NSTARTS*))))
	   (. nstart-text (setText (str @*NSTARTS*)))
	   (let [res (if (< @*NSTARTS* 2) (kmeans (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) @*K* @*MAXITER* @*SNPMODE*) (nth (get-best-clustering (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) @*NSTARTS* @*K* @*MAXITER* @*SNPMODE*) 0))
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
	     (. result-text (setText (reduce #(str (if-not (= nil %1) (str %1 "\n")) (str "Cluster " (inc %2) ": " (count (filter (fn [a] (= %2 a)) (:cluster @*RESULT*))))) nil (range @*K*))))
	     (. statusbar (setText " finished clustering")))

(. run-button (setEnabled true))
(. job-frame (dispose))

	   ))))))))

(. estimate-button
   (addActionListener
    (proxy [ActionListener] []
      (actionPerformed
       [evt]
       (. estimate-button (setEnabled false))
       (let [backgroundExec (java.util.concurrent.Executors/newCachedThreadPool)
	     job-frame (JFrame. "Job running")
	     job-text (JTextField. "Running cluster number estimation...")
	     job-runner (JProgressBar.)
	     job-cancel (JButton. "Stop!")
	     job-panel (JPanel.)

;;running-task (. backgroundExec (submit #^Runnable (fn []
	     running-task (. backgroundExec 
			     (submit 
			      (cast Runnable 
				    (fn []
				      (. statusbar (setText " running cluster number estimation ... this will take some time ..."))
				      (dosync (ref-set *CNEMAX* (try (. Integer (parseInt (. estimate-k-text (getText)))) (catch Exception e 10))))
				      (. estimate-k-text (setText (pr-str @*CNEMAX*)))
				      (dosync (ref-set *CNEMIN* (try (. Integer (parseInt (. estimate-kmin-text (getText)))) (catch Exception e 2))))
				      (. estimate-kmin-text (setText (pr-str @*CNEMIN*)))
				      (dosync (ref-set *ERUNS* (try (. Integer (parseInt (. estimate-run-text (getText)))) (catch Exception e 10))))
				      (. estimate-run-text (setText (pr-str @*ERUNS*)))
				      (dosync (ref-set *MAXITER* (try (. Integer (parseInt (. estimate-maxiter-text (getText)))) (catch Exception e @*MAXITER*))))
				      (. estimate-maxiter-text (setText (pr-str @*MAXITER*)))
				      (dosync (ref-set *NSTARTSCNE* (try (. Integer (parseInt (. nstartcne-text (getText)))) (catch Exception e @*NSTARTSCNE*))))
				      (. nstartcne-text (setText (str @*NSTARTSCNE*)))
				      
				      (let [;;ks (drop 2 (range (inc @*CNEMAX*)))
					    ks (drop @*CNEMIN* (range (inc @*CNEMAX*)))
					    clusterresults (calculate-mca-results (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) @*ERUNS* ks @*MAXITER* @*NSTARTSCNE* @*SNPMODE*)
					    baselineresults (calculate-mca-baselines (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) @*ERUNS* ks @*SNPMODE*)
					    len (count clusterresults)
					    bestk (get-best-k clusterresults baselineresults)
					    res (nth (get-best-clustering (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) @*BESTKRUNS* bestk @*MAXITER* @*SNPMODE*) 0)
					    old (. plot-data (getSeriesCount))]
					(. boxplot-data (clear))
;;					(dorun (map #(.add boxplot-data %1 %2 (if (= %3 bestk) (str %3 "*") %3)) clusterresults (replicate len "McKmeans") (iterate inc 2)))
;;					(dorun (map #(.add boxplot-data %1 %2 (if (= %3 bestk) (str %3 "*") %3)) baselineresults (replicate len "Random prototype baseline") (iterate inc 2)))
					(dorun (map #(.add boxplot-data %1 %2 (if (= %3 bestk) (str %3 "*") %3)) clusterresults (replicate len "McKmeans") ks))
					(dorun (map #(.add boxplot-data %1 %2 (if (= %3 bestk) (str %3 "*") %3)) baselineresults (replicate len "Random prototype baseline") ks))
					(dosync (ref-set *K* bestk))
					(dosync (ref-set *RESULT* res))
					(. numcluster-text (setText (pr-str @*K*)))

					(. result-text (setText (reduce #(str (if-not (= nil %1) (str %1 "\n")) (str "Cluster " (inc %2) ": " (count (filter (fn [a] (= %2 a)) (:cluster @*RESULT*))))) nil (range @*K*))))
					(. statusbar (setText " finished cluster number estimation"))
(JOptionPane/showMessageDialog nil "start RankSumTest" "" JOptionPane/ERROR_MESSAGE)
					(. estimate-result-k (setText (str bestk)))
	     ;; rounding in java - WTF?

					(try
					 (let [rst (RankSumTest.)
					       pval (.pValue rst (double-array (nth clusterresults (- bestk 2))) (double-array (nth baselineresults (- bestk 2))))
					       pval-rd (double (/ (Math/round (* 1000000 pval)) 1000000))]
					  (. estimate-result-text (setText (str pval-rd))))
					 (catch Exception e (. estimate-result-text (setText (str "NA")))))

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
					    (doall (map (fn [idx x] (doto plot-data (. addSeries (str "cluster " idx) x))) (iterate inc 1) (make-plotdata (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) @*DIMX* @*DIMY* @*RESULT*))))))
					    
				      (. estimate-button (setEnabled true))
				      (. job-frame (dispose))))) )]

  (. job-text (setEditable false))
  (. job-runner (setIndeterminate true))
  (. job-cancel
    (addActionListener
     (proxy [ActionListener] []
       (actionPerformed 
	[evt]
        (. running-task (cancel true))
        (. estimate-button (setEnabled true))
	(. job-frame (dispose))))))
  (doto job-panel
    (. setLayout (FlowLayout.))
    (. add job-runner)
    (. add job-cancel))
  (doto job-frame
    (. setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
;    (. setSize 10 100)
    (. setLayout (BorderLayout.))
    (. add job-text BorderLayout/NORTH)
    (. add job-panel BorderLayout/CENTER)
    (. pack)
    (. setVisible true))


	   )))))



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
		  dataset (if-not snp (read-csv filename "," false csv-parse-double) (read-csv filename "," false csv-parse-int))
		  old (. plot-data (getSeriesCount))]

	      (dosync (ref-set *SNPMODE* snp))
	      (dosync (ref-set *DATASET* dataset))
;;(JOptionPane/showMessageDialog nil (str "hier") "" JOptionPane/ERROR_MESSAGE)
	      (dosync (ref-set *TDATASET* (transpose dataset snp)))
	      (dosync (ref-set *TRANSPOSED* false))
              (dosync (ref-set *RESULT* nil))
              (dosync (ref-set *DIMX* 0))
              (dosync (ref-set *DIMY* 1))
              (. dimx-text (setText (str (inc @*DIMX*))))
              (. dimy-text (setText (str (inc @*DIMY*))))
;;(JOptionPane/showMessageDialog nil (str "hier") "" JOptionPane/ERROR_MESSAGE)
              ;set axis label
              (.. plot-area getXYPlot getDomainAxis (setLabel (str "column " (inc @*DIMX*))))
              (.. plot-area getXYPlot getRangeAxis (setLabel (str "column " (inc @*DIMY*))))

	      (. info-sample-text (setText (str (count dataset))))
	      (. info-feature-text (setText (str (alength (first dataset)))))

	      (if @*SNPMODE*
		(do ; hide sammon-button
		  (. sammon-button (setVisible false))
                  ; hide dimx, dimy selection
                  (. dimx-label (setVisible false))
                  (. dimy-label (setVisible false))
                  (. dimx-text (setVisible false))
                  (. dimy-text (setVisible false))
                  (. dim-button (setVisible false))
		  ; remove old plots
		  (let [tmp (. kmodes-combined-plot (getSubplots))]
		    (dotimes [i (. tmp (size))]
		      (. kmodes-combined-plot (remove (. tmp (get 0))))))
	          ; add new plot
		  (. kmodes-combined-plot (add (create-kmodes-cluster-plot (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*))))
		  (. plot-panel (setChart kmodes-combined-chart))
		  (. plot-panel (setRangeZoomable false)))

;(JOptionPane/showMessageDialog nil (str ) "" JOptionPane/ERROR_MESSAGE)
		(do ;show sammon-button
		  (. sammon-button (setVisible true))
                  ; show dimx, dimy selection
                  (. dimx-label (setVisible true))
                  (. dimy-label (setVisible true))
                  (. dimx-text (setVisible true))
                  (. dimy-text (setVisible true))
                  (. dim-button (setVisible true))

		  (. plot-panel (setChart plot-area))
		    (. plot-panel (setRangeZoomable true))
		    (doall (map (fn [idx] (doto plot-data (. removeSeries (str "cluster " idx)))) (drop 1 (range (inc old)))))
		    (doall (map (fn [idx x] (doto plot-data (. addSeries (str "cluster " idx) x))) (iterate inc 1) (data2plotdata (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) @*DIMX* @*DIMY*)))))

	      (. boxplot-data (clear))
	      (. result-text (setText (str "Cluster 1: " (count dataset))))
	      (. statusbar
		 (setText " file loaded")))
	    (catch Exception e (JOptionPane/showMessageDialog nil (str e) "Error" JOptionPane/ERROR_MESSAGE)))))))
;	    (catch Exception e (JOptionPane/showMessageDialog nil "Error while loading file. See 'Help - File format'\nfor information about supported formats." "Error" JOptionPane/ERROR_MESSAGE)))))))

    (. menu-file-save
       (addActionListener
	(proxy [ActionListener] []
	  (actionPerformed 
	   [evt]
	   (try
	    (let [ret (. file-chooser-csv (showSaveDialog frame))
		  filename (. (. file-chooser-csv (getSelectedFile)) (getPath))
;					      restext (.. (pr-str (:cluster @*RESULT*)) (replace "(" "") (replace ")" ""))]
		  res (:cluster @*RESULT*)]
;					  (save-result filename restext)
	      (write-csv res filename "," false list-parse-csv)
	      (. statusbar (setText " result saved")))
	    (catch Exception e (. statusbar (setText " error while saving file"))))))))

    (. menu-export-svg
       (addActionListener
	(proxy [ActionListener] []
	  (actionPerformed 
	   [evt]
	   (try
	    (let [which-chart (. tabbed-pane (getSelectedIndex))
		  ret (. file-chooser-svg (showSaveDialog frame))
		  filename (. (. file-chooser-svg (getSelectedFile)) (getPath))]
	      (if (== which-chart 0)
		(exportChart2SVG plot-area frame filename)
		(exportChart2SVG boxplot-chart frame filename)))
	    (catch Exception e nil))))))

    (. menu-preferences
       (addActionListener
	(proxy [ActionListener] []
	  (actionPerformed 
	   [evt]
	   (show-preferences-panel)))))
		
    (. menu-exit
       (addActionListener
	(proxy [ActionListener] []
	  (actionPerformed 
	   [evt]
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
		(. menu-help-help
		   (addActionListener
		    (proxy [ActionListener] []
			   (actionPerformed [evt]
					    (show-help-panel)))))
	
		(doto menu-help
		  (. add menu-help-help)
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

;    (. result-text (setEditable false))
;    (. result-text (setLineWrap true))
;    (doto result-panel
;      (. setLayout (new BorderLayout))
      ;(. add result-label (. BorderLayout NORTH))
;      (. add result-scrollpane (. BorderLayout CENTER)))
;      (. add statusbar (. BorderLayout SOUTH)))
;(. result-list addElement (str "Cluster 0: "))
(. result-text (setEditable false))
(. result-text (setLineWrap true))
;(. result-text (setText "Cluster 0: \nCluster 1: \nCluster 2: \nCluster 3: \n"))
(. result-text (setText "Cluster 1:"))
;(doto result-text
;  (. setLayoutOrientation JList/VERTICAL)
;  (. setVisibleRowCount 1)
;  (. setSelectedIndex 0))

(. dimx-text (setText (str (inc @*DIMX*))))
(. dimy-text (setText (str (inc @*DIMY*))))
(. dim-button
   (addActionListener
    (proxy [ActionListener] []
      (actionPerformed
       [evt]
       (dosync (ref-set *DIMX* (try (let [newx (dec (. Integer (parseInt (. dimx-text (getText)))))]
				      (if (or (< newx 0) (> newx (dec (. Integer (parseInt (. info-feature-text (getText))))))) @*DIMX* newx)) (catch Exception e @*DIMX*))))
       (dosync (ref-set *DIMY* (try (let [newy (dec (. Integer (parseInt (. dimy-text (getText)))))]
				      (if (or (< newy 0) (> newy (dec (. Integer (parseInt (. info-feature-text (getText))))))) @*DIMY* newy)) (catch Exception e @*DIMY*))))
       (. dimx-text (setText (str (inc @*DIMX*))))
       (. dimy-text (setText (str (inc @*DIMY*))))
; set axis label
(.. plot-area getXYPlot getDomainAxis (setLabel (str "column " (inc @*DIMX*))))
(.. plot-area getXYPlot getRangeAxis (setLabel (str "column " (inc @*DIMY*))))
; remove old plots
       (doall (map (fn [idx] (doto plot-data (. removeSeries (str "cluster " idx)))) (drop 1 (range (inc (. plot-data (getSeriesCount)))))))
; add new plots
(if (= nil @*RESULT*)
  (doall (map (fn [idx x] (doto plot-data (. addSeries (str "cluster " idx) x))) (iterate inc 1) (data2plotdata (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) @*DIMX* @*DIMY*)))
  (doall (map (fn [idx x]
		(doto plot-data (. addSeries (str "cluster " idx) x))) (iterate inc 1) (make-plotdata (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) @*DIMX* @*DIMY* @*RESULT*))))
       ))))

		(let [layout (GroupLayout. kmeans-options-panel)
		      parGrouplabelh (. layout (createParallelGroup))
		      parGrouplabel2h (. layout (createParallelGroup))
		      parGrouptexth (. layout (createParallelGroup))
		      parGrouptext2h (. layout (createParallelGroup))
		      parGrouplabelv (. layout (createParallelGroup))
		      parGrouptextv (. layout (createParallelGroup))
		      parGroupnstartv (. layout (createParallelGroup))
		      parGroupbuttonsv (. layout (createParallelGroup))
		      seqGrouph (. layout (createSequentialGroup))
		      seqGroupv (. layout (createSequentialGroup))]
		  (. layout setAutoCreateGaps true)
		  (. layout setAutoCreateContainerGaps true)
		  (doto parGrouplabelh
		    (. addComponent numcluster-label)
		    (. addComponent maxiter-label)
		    (. addComponent nstart-label))
		  (doto parGrouptexth
		    (. addComponent numcluster-text
		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
		    (. addComponent maxiter-text 
		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
		    (. addComponent nstart-text
		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
		    (. addComponent run-button
		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE))
(doto parGrouplabel2h
  (. addComponent result-label)
  (. addComponent dimx-label)
  (. addComponent dimy-label))
(doto parGrouptext2h
  (. addComponent result-scrollpane
     GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
  (. addComponent dimx-text
     GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
  (. addComponent dimy-text
     GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
  (. addComponent dim-button))
		  (doto seqGrouph
		    (. addGroup parGrouplabelh)
		    (. addGroup parGrouptexth)
;		    (. addComponent result-label)
;		    (. addComponent result-scrollpane
;		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE))
(. addGroup parGrouplabel2h)
(. addGroup parGrouptext2h))
		  (. layout setHorizontalGroup seqGrouph)

		  (doto parGrouplabelv
		    (. addComponent numcluster-label)
		    (. addComponent numcluster-text
		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
		    (. addComponent result-label)
		    (. addComponent result-scrollpane
		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE))
		  (doto parGrouptextv
		    (. addComponent maxiter-label)
		    (. addComponent maxiter-text
		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
(. addComponent dimx-label)
(. addComponent dimx-text
   GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE))
		  (doto parGroupnstartv
		    (. addComponent nstart-label)
		    (. addComponent nstart-text
		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
(. addComponent dimy-label)
(. addComponent dimy-text
   GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE))
(doto parGroupbuttonsv
  (. addComponent run-button)
  (. addComponent dim-button))
 		  (doto seqGroupv
 		    (. addGroup parGrouplabelv)
 		    (. addGroup parGrouptextv)
		    (. addGroup parGroupnstartv)
; 		    (. addComponent run-button)
(. addGroup parGroupbuttonsv))
		  (. layout setVerticalGroup seqGroupv)
		  (. kmeans-options-panel setLayout layout))

(. estimate-result-text (setEditable false))
(. estimate-result-k (setEditable false))

		(let [layout (GroupLayout. cne-options-panel)
		      parGrouplabelh (. layout (createParallelGroup))
		      parGrouptexth (. layout (createParallelGroup))
parGroupestimateh (. layout (createParallelGroup))
parGroupestimatelabelh (. layout (createParallelGroup))
parGroupkminv (. layout (createParallelGroup))
		      parGrouplabelv (. layout (createParallelGroup))
		      parGrouptextv (. layout (createParallelGroup))
		      parGrouprunv (. layout (createParallelGroup))
		      seqGrouph (. layout (createSequentialGroup))
		      seqGroupv (. layout (createSequentialGroup))]
		  (. layout setAutoCreateGaps true)
		  (. layout setAutoCreateContainerGaps true)
		  (doto parGrouplabelh
(. addComponent estimate-kmin-label)
		    (. addComponent estimate-k-label)
		    (. addComponent estimate-maxiter-label)
		    (. addComponent estimate-run-label))
		  (doto parGrouptexth
(. addComponent estimate-kmin-text
   GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
		    (. addComponent estimate-k-text
		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
		    (. addComponent estimate-maxiter-text 
		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
		    (. addComponent estimate-run-text
		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
		    (. addComponent estimate-button
		       GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE))

(doto parGroupestimateh
  (. addComponent nstartcne-text
     GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
  (. addComponent estimate-result-k
     GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE)
  (. addComponent estimate-result-text
     GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE))

(doto parGroupestimatelabelh
  (. addComponent nstartcne-label)
  (. addComponent estimate-result-klabel)
  (. addComponent estimate-result-label))

		  (doto seqGrouph
		    (. addGroup parGrouplabelh)
		    (. addGroup parGrouptexth)
(. addGroup parGroupestimatelabelh)
(. addGroup parGroupestimateh))
		  (. layout setHorizontalGroup seqGrouph)

(doto parGroupkminv
(. addComponent estimate-kmin-label)
(. addComponent estimate-kmin-text
   GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE))
		  (doto parGrouplabelv
		    (. addComponent estimate-k-label)
		    (. addComponent estimate-k-text)
(. addComponent nstartcne-label)
(. addComponent nstartcne-text
   GroupLayout/PREFERRED_SIZE GroupLayout/DEFAULT_SIZE GroupLayout/PREFERRED_SIZE))
		  (doto parGrouptextv
		    (. addComponent estimate-maxiter-label)
		    (. addComponent estimate-maxiter-text)
(. addComponent estimate-result-klabel)
(. addComponent estimate-result-k))
		  (doto parGrouprunv
		    (. addComponent estimate-run-label)
		    (. addComponent estimate-run-text)
(. addComponent estimate-result-label)
(. addComponent estimate-result-text))



		  (doto seqGroupv
(. addGroup parGroupkminv)
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
;		  (. setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
		  (. setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
		  (. setVisible true))))


(defn runKmeans [outfile]
  (let [res (if (= 1 @*NSTARTS*) (kmeans (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) @*K* @*MAXITER* @*SNPMODE*) (nth (get-best-clustering (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) @*NSTARTS* @*K* @*MAXITER* @*SNPMODE*) 0))
	restext (.. (pr-str (:cluster res)) (replace "(" "") (replace ")" ""))]
    (save-result outfile restext)))

(defn runCNE [outfile cneoutfile cneplot cneplotfile]
  (let [ks (drop @*CNEMIN* (range (inc @*CNEMAX*)))
	clusterresults (do ;(println "start cluster loop")
			   (calculate-mca-results @*DATASET* @*ERUNS* ks @*MAXITER* @*NSTARTSCNE* @*SNPMODE*))
	baselineresults (do ;(println "start baseline loop")
			    (calculate-mca-baselines @*DATASET* @*ERUNS* ks @*SNPMODE*))
	bestk (do ;(println "get best k")
		  (get-best-k clusterresults baselineresults))
	res (do ;(println "get best clustering")
		(nth (get-best-clustering (if-not @*TRANSPOSED* @*DATASET* @*TDATASET*) @*BESTKRUNS* bestk @*MAXITER* @*SNPMODE*) 0))
	restext (do ;(println "formating result")
		    (.. (pr-str (:cluster res)) (replace "(" "") (replace ")" "")))]
;;    (println "save runs")
    (save-clusternumber-result clusterresults baselineresults cneoutfile)
;;    (println "save best run")
    (save-result outfile restext)
;;    (println "save plot")
    (if cneplot
      (boxplot-clusternumber clusterresults baselineresults bestk cneplotfile))))

(defn runCMD [infile outfile k maxiter nstart cne? cnemin cnemax cneruns cnenstart cneoutfile cneplot cneplotfile]
  (dosync (ref-set *SNPMODE* (. (. infile (substring (inc (. infile (lastIndexOf "."))))) (equalsIgnoreCase "snp"))))
; (dosync (ref-set *DATASET* (load-tab-file infile @*SNPMODE*)))
  (dosync (ref-set *DATASET* (if-not @*SNPMODE* (read-csv infile "," false csv-parse-double) (read-csv infile "," false csv-parse-int))))
  (if (string? k)
    (dosync (ref-set *K* (. Integer (parseInt k))))
    (dosync (ref-set *K* k)))
  (if (string? maxiter)
    (dosync (ref-set *MAXITER* (. Integer (parseInt maxiter))))
    (dosync (ref-set *MAXITER* maxiter)))
  (if (string? nstart)
    (dosync (ref-set *NSTARTS* (. Integer (parseInt nstart))))
    (dosync (ref-set *NSTARTS* nstart)))
  (if (string? cneruns)
    (dosync (ref-set *ERUNS* (. Integer (parseInt cneruns))))
    (dosync (ref-set *ERUNS* cneruns)))
  (if (string? cnemin)
    (dosync (ref-set *CNEMIN* (. Integer (parseInt cnemin))))
    (dosync (ref-set *CNEMIN* cnemin)))
  (if (string? cnemax)
    (dosync (ref-set *CNEMAX* (. Integer (parseInt cnemax))))
    (dosync (ref-set *CNEMAX* cnemax)))
  (if (string? cnenstart)
    (dosync (ref-set *NSTARTSCNE* (. Integer (parseInt cnenstart))))
    (dosync (ref-set *NSTARTSCNE* cnenstart)))
  (if cne?
    (do (println "Starting McKmeans cluster number estimation."); Find the output in " outfile " and " cneoutfile)
	(runCNE outfile cneoutfile cneplot cneplotfile))
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
       [nstart "The number of K-means restarts. If set > 1 the best result from these repeated runs is reported." 1]
       [cne? "Run a cluster number estimation? This is a boolean flag."]
       [cnemin "The minimal number of clusters for the cluster number estimation." 2]
       [cnemax "The maximal number of clusters for the cluster number estimation." 10]
       [cneruns "The number of repeated runs of clusterings for each partitioning." 10]
       [cnenstart "The number of K-means restarts in cluster number estimation. If set > 1 only the best result from these runs is included." 10]
       [cneoutfile co "The name of the output file for the cluster number estimation." "cneresult.txt"]
       [cneplot? "Plot the result of the cluster number estimation to file? This is a boolean flag."]
       [cneplotfile "The name of the plot file for boxplots from cluster number estimation." "cneplot.svg"]]
      (if (nil? infile)
	(println "Please provide a valid input file.")
	(runCMD infile outfile k maxiter nstart cne? cnemax cneruns cnenstart cneoutfile cneplot? cneplotfile)))))
