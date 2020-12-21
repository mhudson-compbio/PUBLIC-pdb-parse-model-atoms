
;;;;;; Absolute superfund site of primitives / utility functions that form the basis of the DSL, many from other projects -- need to refactor for this in the context of the  combined project
;;; These should mostly run fine from the REPL but need to be cleaned up for compiling/using in libraries, a lot of this can be condensed/improved, keep for reference


;;;;(ns gh.core
;;;;  (:require [instaparse.core :as insta]
;;;;            [clojure.java.io :as io]
;;;;            [clojure.edn :as edn]
;;;;            [gh.output :as output]))

(def dir (io/file "/pdb/"))

(defn pdb-ids []
  (map (partial apply str)
       (map (partial take 4)
            (filter #(re-find #".pdb" %)
                    (for [file (file-seq dir)]
                      (.getName file))))))

(defn copy-pdb-file! [pdb]
 (let [base-url "http://www.rcsb.org/pdb/files/"
       directory dir]
  (with-open [in (io/input-stream (str base-url pdb ".pdb"))
              out (io/output-stream (str directory pdb ".pdb"))]
    (io/copy in out))))

(defn copy-mult-pdb! [pdb-sequence]
  (if (first pdb-sequence)
    (cons (copy-pdb-file! (first pdb-sequence))
          (copy-mult-pdb! (rest pdb-sequence)))
    (list)))

(defn load-pdb [pdb]
  (let [directory "/pdb/"
        choose-file (fn [x] (str directory x ".pdb"))]
    (with-open [rdr (io/reader (choose-file pdb))]
      (doall (line-seq rdr)))))

(defn pdb-map
  [pdb]
  (let [data (load-pdb pdb)
        chunk (fn [l] (apply str (take 4 l)))
        flt (fn [t] (vec (filter #(= (chunk %) t) data)))]
    (into {} (map #(hash-map (keyword %)
                             (flt (identity %))) ["HEAD" "TITL" "COMP" "SOUR"
                                                  "REMA" "ATOM" "KEYW" "EXPD"
                                                  "AUTH" "REVD" "JRNL" "DBRE"
                                                  "SEQA" "SEQR" "FORM" "HELI"
                                                  "SHEE" "LINK" "CISP" "SITE"
                                                  "HETA" "CONE"]))))

;;;; Basic context free grammars for parsing, these should cover most use basic cases but others can be added fairly easily

(def header-parser
  (insta/parser
   " line = record <ws> classification <ws> date <ws> id <ws>?;
     record = 'HEADER';
     classification = #'\\w*';
     <ws> = #'\\s*';
     date = #'\\w+';
     id   = #'\\w+';"))

(def atom-parser
  (insta/parser
   " line = record <ws> serial <ws> aname <ws>? alt? <ws>? resid <ws> chain <ws> seqnum <ws> x <ws> y <ws> z <ws> occup <ws> tempfa <ws> elemn <ws>?;
     record = 'ATOM';
     <ws> = #'\\s*';
     serial = #'[0-9]+';
     aname = 'N'|'NA'|'NA1'|'NA2'|'NB'|'NB1'|'NB2'|'NG'|'NG1'|'NG2'|'ND'|'ND1'|'ND2'|'NE'|'NE1'|'NE2'|'NZ'|'NZ1'|'NZ2'|'NH'|'NH1'|'NH2'|'C'|'CA'|'CA1'|'CA2'|'CB'|'CB1'|'CB2'|'CG'|'CG1'|'CG2'|'CD'|'CD1'|'CD2'|'CE'|'CE1'|'CE2'|'CE3'|'CZ'|'CZ1'|'CZ2'|'CZ3'|'CH'|'CH1'|'CH2'|'O'|'OA'|'OA1'|'OA2'|'OB'|'OB1'|'OB2'|'OG'|'OG1'|'OG2'|'OD'|'OD1'|'OD2'|'OE'|'OE1'|'OE2'|'OZ'|'OZ1'|'OZ2'|'OH'|'OH1'|'OH2'|'OXT'|'HXT'|'SD'|'SG'|'H'|'HH'|'HG'|'HZ1'|'HZ2'|'HZ3'|'H1'|'H2'|'H3';
     alt = 'A' | 'B';
     resid = 'ALA'|'ARG'|'ASN'|'ASP'|'CYS'|'GLU'|'GLN'|'GLY'|'HIS'|'ILE'|'LEU'|'LYS'|'MET'|'PHE'|'PRO'|'SER'|'THR'|'TRP'|'TYR'|'VAL';
     chain = 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'U';
     seqnum = #'[0-9]+';
     x = '-'? #'[0-9]+' '.' #'[0-9]+';
     y = '-'? #'[0-9]+' '.' #'[0-9]+';
     z = '-'? #'[0-9]+' '.' #'[0-9]+';
     occup = '-'? #'[0-9]+' '.' #'[0-9]+';
     tempfa = '-'? #'[0-9]+' '.' #'[0-9]+';
     elemn = 'C' | 'N' | 'O' | 'S' | 'H';"))

(defn- transform-atom-parse [x]
  (let [compfn (fn [k] (comp #(vector (identity k) %) edn/read-string str))]
    (insta/transform
     {:serial (compfn :serial)
      :seqnum (compfn :seqnum)
      :x      (compfn :x)
      :y      (compfn :y)
      :z      (compfn :z)
      :occup  (compfn :occup)
      :tempfa (compfn :tempfa)}
     x)))

(defn parse-atom-pdb [pdb]
  (map transform-atom-parse (map atom-parser (:ATOM (pdb-map pdb)))))

(defn atoms-data [pdb]
  (map #(into {} %)
       (map #(filter (partial vector?) %)
            (parse-atom-pdb pdb))))

;; (defn parse-output
;;   [& {:keys [test atom head titl comp]
;;       :or {test "./resources/parse-output/test/"}}]
;;   (output/spitcol "/reso"))

(defn atoms-parse-output
  ;; ./resources/parse-output/atom/
  [pdb]
  (with-open [w (clojure.java.io/output-stream (str "./resources/parse-output/atom/" pdb))]
    (doseq [line (atoms-data pdb)]
      (.write w line)
      (.newLine w))))

(defn parse-output
  [pdb]
  (with-open [in (io/reader (atoms-data pdb))
              out (io/output-stream (str "/resources/parse-output/atom/" pdb))]
    (io/copy in out)))

(defn aggregate-atom-data [pdb]
  "Given a .pdb file, returns a map. The map
   contains key value pairs corresponding to
   aggregate collections of particular values
   of atomic properties accross all atoms in
   the structure.

   For example, all x coordinate values for
   every atom in the structure

    => (:x (aggregate '1aon))
    >  (63.13 63.914 63.317 ...)"
  (let [data   (atoms-data pdb)
        aggr (fn [k] (pmap k data))]
    {:chain     (aggr :chain)
     :seqnum    (aggr :seqnum)
     :x         (aggr :x)
     :y         (aggr :y)
     :z         (aggr :z)
     :tempfa    (aggr :tempfa)
     :elemn     (aggr :elemn)
     :aname     (aggr :aname)
     :resid     (aggr :resid)
     :record    (aggr :record)
     :occup     (aggr :occup)
     :serial    (aggr :serial)}))

(def hetatm-parser
"context free grammars are pretty useful in situations like dealing with hetatm records which can be very inconsistent, but even this parser has issues and needs to be updated,
it should work fine for most PDB sourced hetatms, but still issues with many thirdparty generated ones e.g. COFACTOR"
  (insta/parser
   " line = record <ws>? serial <ws> aname <ws>? alt? <ws>? resid <ws> chain? <ws>? seqnum <ws> x <ws> y <ws> z <ws> occup? <ws>? tempfa? <ws>? elemn <ws>?;
     record = 'HETATM';
     <ws> = #'\\s*';
     serial = #'[0-9]+';
     aname = #'[a-zA-Z0-9\\']{1,4}';
     alt = 'A' | 'B';
     resid = 'ALA'|'ARG'|'ASN'|'ASP'|'CYS'|'GLU'|'GLN'|'GLY'|'HIS'|'ILE'|'LEU'|'LYS'|'MET'|'PHE'|'PRO'|'SER'|'THR'|'TRP'|'TYR'|'VAL'|#'[a-zA-Z0-9]{1,3}';
     chain = 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'U' | 'Y' | 'N';
     seqnum = #'[0-9]+';
     x = '-'? #'[0-9]+' '.' #'[0-9]{3}';
     y = '-'? #'[0-9]+' '.' #'[0-9]{3}';
     z = '-'? #'[0-9]+' '.' #'[0-9]{3}';
     occup = '-'? #'[0-9]+' '.' #'[0-9]{2}';
     tempfa = '-'? #'[0-9]+' '.' #'[0-9]+';
     elemn = 'C' | 'N' | 'O' | 'S' | 'H' | 'F' | 'P' | 'I' | 'CL' | 'FE';"))

(defn- transform-hetatm-parse [x]
  (let [compfn (fn [k] (comp #(vector (identity k) %) edn/read-string str))]
    (insta/transform
     {:serial (compfn :serial)
      :seqnum (compfn :seqnum)
      :x      (compfn :x)
      :y      (compfn :y)
      :z      (compfn :z)
      :occup  (compfn :occup)
      :tempfa (compfn :tempfa)}
     x)))

(defn parse-hetatm-pdb [pdb]
  (map transform-hetatm-parse (map hetatm-parser (:HETA (pdb-map pdb)))))

(defn hetatm-data [pdb]
  (map #(into {} %)
       (map #(filter (partial vector?) %)
            (parse-hetatm-pdb pdb))))

(defn pp-hetatm [pdb]
  (clojure.pprint/pprint (take 3 (shuffle (hetatm-data pdb)))))

(defn aggregate-hetatm-data [pdb]
  (let [data   (hetatm-data pdb)
        aggr (fn [k] (pmap k data))]
    {:chain     (aggr :chain)
     :seqnum    (aggr :seqnum)
     :x         (aggr :x)
     :y         (aggr :y)
     :z         (aggr :z)
     :tempfa    (aggr :tempfa)
     :elemn     (aggr :elemn)
     :aname     (aggr :aname)
     :resid     (aggr :resid)
     :record    (aggr :record)
     :occup     (aggr :occup)
     :serial    (aggr :serial)}))

(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn round2
  "Round a double to the given precision (number of significant digits)
   https://stackoverflow.com/questions/10751638/clojure-rounding-to-decimal-places"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn hetatm-centroid [pdb]
  {:x (mean (:x (aggregate-hetatm-data pdb)))
   :y (mean (:y (aggregate-hetatm-data pdb)))
   :z (mean (:z (aggregate-hetatm-data pdb)))})

(defn hetatm-centroid-alt [pdb]
  {:id pdb
   :x (round2 3 (mean (:x (aggregate-hetatm-data pdb))))
   :y (round2 3 (mean (:y (aggregate-hetatm-data pdb))))
   :z (round2 3 (mean (:z (aggregate-hetatm-data pdb))))})


(defmacro swallow-exceptions [& body]
  "As seen on Stack Overflow! --- delete this macro probably"
  `(try ~@body (catch Exception e#)))

(defmacro maybe
  "Assuming that the body of code returns X, this macro returns [X nil] in the case of no error
  and [nil E] in event of an exception object E.

https://bitumenframework.blogspot.com/2010/11/non-breaking-error-handling-in-clojure.html"
  [& body]
  `(try [(do ~@body) nil]
     (catch Exception e#
       [nil e#])))

(defn brute-force-hetatm-centroid-output []
  (swallow-exceptions (map #(spit (str hdir "/" %) (maybe (hetatm-centroid %)))
                           (doall (pdb-bs-ids)))))




;;; (import other utility libraries)


;;;; other utility primatives for collections

(defn read-file-by-line
  [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (doall (line-seq rdr))))

(defn seefreq
  [col]
  (reverse (sort-by val (frequencies col))))

(defn spitcol
  [filename collection]
  (spit filename
        (clojure.string/join (map #(str % "\n") collection))))

;;;;(ns poser.core
  ;;;;(:require poser.utility :as util))

(defn readf
  [file]
  (util/read-file-by-line file))

(defn col->file
  [col fname]
  (util/spitcol fname col))

;;;; (ns poser.visualize
  ;;;; (:require [quil.core :as quil]))


;;;; (ns lh.prototype.pdb-data
 ;;;; (:require [lh.primitives.parse.pdb :as pdb-parsing]
  ;;;;          [lh.prototype.pdb-download :refer :all]
  ;;;;          [clojure.java.io :refer [as-file]]
  ;;;;          [criterium.core :as crit]
  ;;;;          [incanter.core :as icore :refer [view]]
  ;;;;          [incanter.stats :as incanter-stats]
  ;;;;          [incanter.charts :as charts :refer [bar-chart]]))


;;;; lh

(defn load-pdb-path [pdb]
  (if (true?
       (.exists (as-file
                 (str "data/pdb/" pdb ".pdb"))))

    (pdb-parsing/select-file! (str pdb ".pdb"))

    (do (copy-pdb-file! pdb)
        (pdb-parsing/select-file! (str pdb ".pdb")))))


(defn get-atom-data [pdb]
  (pdb-parsing/parse-trns-pdb-atom (load-pdb-path pdb)))


(defn data-fields-from-atom [pdb]
  (map #(filter (partial vector?) %) (get-atom-data pdb)))


(defn lazy-mapified-atom-data [pdb]
  (map #(into {} %) (data-fields-from-atom pdb)))


(defn realized-mapified-atom-data [pdb]
  (mapv #(into {} %) (data-fields-from-atom pdb)))

(defmacro reu [namespace]
  "Macro for reloading namespaces, should be stored in an easy to quicktype
   var defined in replspace. As seen on stackoverflow!"
  `(-> (quote ~namespace) use))

(defn drop-if [value collection]
  (filter #(not (= value %)) collection))

(defn drop-if-not [value collection]
  (filter #(= value %) collection))

(defn drop-if-less [value collection]
  (filter #(< value %) collection))

(defn drop-if-greater [value collection]
  (filter #(> value %) collection))

(defn drop-nil [collection]
  (filter #(not (= nil %)) collection))

(defn partial-keylook [key]
  (fn [] (partial key)))

(defn map-keylook [key collection]
  (map #(((partial-keylook key)) %) collection))

(defn map-keylook-chain [pdb-data]
  (map-keylook :chain pdb-data))

(defn map-keylook-seqnum [pdb-data]
  (map-keylook :seqnum pdb-data))

(defn map-keylook-x [pdb-data]
  (map-keylook :x pdb-data))

(defn map-keylook-y [pdb-data]
  (map-keylook :y pdb-data))

(defn map-keylook-z [pdb-data]
  (map-keylook :z pdb-data))

(defn map-keylook-tempfact [pdb-data]
  (map-keylook :temp-fact pdb-data))

(defn map-keylook-element [pdb-data]
  (map-keylook :elemn pdb-data))

(defn map-keylook-aname [pdb-data]
  (map-keylook :aname pdb-data))

(defn map-keylook-residue [pdb-data]
  (map-keylook :resid pdb-data))

(defn map-keylook-record [pdb-data]
  (map-keylook :record pdb-data))

(defn map-keylook-occupancy [pdb-data]
  (map-keylook :occupancy pdb-data))

(defn map-keylook-serial [pdb-data]
  (map-keylook :serial pdb-data))

(defn distinct-count [collection]
  (count (distinct collection)))

(defn distinct-drop-count [drop-item collection]
  (distinct-count (drop-if drop-item collection)))

(defn distinct-keylook-count-in-subseq [key sequence]
  (distinct-count (map-keylook key sequence)))

(defn distinct-keylook-drop-count-of-subseq [key drop-item sequence]
  (distinct-drop-count drop-item (map-keylook key sequence)))

(defn distinct-of-subseq [key sequence]
  (distinct (map-keylook key sequence)))

(defn dropnil-count-atoms [pdb-data]
  (count (drop-nil (map-keylook-element pdb-data))))

(defn distinct-count-chains [pdb-data]
  (distinct-count (map-keylook-chain pdb-data)))

(defn distinct-count-amino-acids [pdb-data]
  (distinct-count (map-keylook-seqnum pdb-data)))

(defn distinct-count-residue-type [pdb-data]
  (distinct-count (map-keylook-residue pdb-data)))

(defn distinct-elements [pdb-data]
  (distinct (map-keylook-element pdb-data)))

(defn distinct-elements2 [pdb-data]
  (distinct-of-subseq :elemn pdb-data))

(defn distinct-element-drop-count [pdb-data]
  (distinct-count (drop-if nil (map-keylook-element pdb-data))))

(defn count-carbon [pdb-data]
  (count (drop-if-not "C" (map-keylook-element pdb-data))))

(defn count-nitrogen [pdb-data]
  (count (drop-if-not "N" (map-keylook-element pdb-data))))

(defn count-oxygen [pdb-data]
  (count (drop-if-not "O" (map-keylook-element pdb-data))))

(defn count-sulfur [pdb-data]
  (count (drop-if-not "S" (map-keylook-element pdb-data))))

(defn count-phosphorus [pdb-data]
  (count (drop-if-not "P" (map-keylook-element pdb-data))))

(defn carbon-ratio [pdb-data]
  (/ (count-carbon pdb-data) (drop-count-atoms pdb-data)))

(defn nitrogen-ratio [pdb-data]
  (/ (count-nitrogen pdb-data) (drop-count-atoms pdb-data)))

(defn oxygen-ratio [pdb-data]
  (/ (count-oxygen pdb-data) (drop-count-atoms pdb-data)))

(defn sulfur-ratio [pdb-data]
  (/ (count-sulfur pdb-data) (drop-count-atoms pdb-data)))

(defn phosphorus-ratio [pdb-data]
  (/ (count-phosphorus pdb-data) (drop-count-atoms pdb-data)))

(defn percent-carbon [pdb-data]
  (* (carbon-ratio pdb-data) 100.0))

(defn percent-nitrogen [pdb-data]
  (* (nitrogen-ratio pdb-data) 100.0))

(defn percent-oxygen [pdb-data]
  (* (oxygen-ratio pdb-data) 100.0))

(defn percent-sulfur [pdb-data]
  (* (sulfur-ratio pdb-data) 100.0))

(defn percent-phosphorus [pdb-data]
  (* (phosphorus-ratio pdb-data) 100.0))

(defn sort-by-chain [pdb-data]
  (sort-by :chain pdb-data))

(defn sort-by-seqnum [pdb-data]
  (sort-by :seqnum pdb-data))

(defn sort-by-elemn [pdb-data]
  (sort-by :elemn pdb-data))

(defn sort-by-aname [pdb-data]
  (sort-by :aname pdb-data))

(defn sort-by-residue [pdb-data]
  (sort-by :residue pdb-data))

(defn sort-by-record [pdb-data]
  (sort-by :record pdb-data))

(defn sort-by-occupancy [pdb-data]
  (sort-by :occupancy pdb-data))

(defn sort-by-serial [pdb-data]
  (sort-by :serial pdb-data))

(defn sort-by-x [pdb-data]
  (sort-by :x pdb-data))

(defn sort-by-y [pdb-data]
  (sort-by :y pdb-data))

(defn sort-by-z [pdb-data]
  (sort-by :z pdb-data))

(defn filter-keylook-pick [key value collection]
  (filter #(= (((partial-keylook key)) %) value) collection))

(defn between? [checknum range-delimit-one range-delimit-two]
  (if (or (and (< checknum range-delimit-one) (> checknum range-delimit-two))
          (and (> checknum range-delimit-one) (< checknum range-delimit-two)))
    true
    false))

(defn filter-keylook-range-pick [key start-range end-range collection]
  (filter #(= (between?
               (((partial-keylook key)) %)
               start-range
               end-range)
              true)
          collection))

(defn pick-by [key val collection]
  (if (= 1 (count (vec (filter-keylook-pick key val collection))))
    (first (vec (filter-keylook-pick key val collection)))
    (vec (filter-keylook-pick key val collection))))

(defn pick-by-range [key start end collection]
  (if (= 1 (count (vec filter-keylook-range-pick key start end collection)))
    (first (vec (filter-keylook-range-pick key start end collection)))
    (vec (filter-keylook-range-pick key start end collection))))

(defn pick-by-serial [serial pdb-data]
  (pick-by :serial serial pdb-data))

(defn pick-by-serial-range [start end pdb-data]
  (pick-by-range :serial start end pdb-data))

(defn pick-by-seqnum [seqnum pdb-data]
  (pick-by :seqnum seqnum pdb-data))

(defn pick-by-seqnum-range [start end pdb-data]
  (pick-by-range :seqnum start end pdb-data))

(defn pick-by-chain [chain pdb-data]
  (pick-by :chain chain pdb-data))

(defn pick-by-element [element pdb-data]
  (pick-by :elemn element pdb-data))

(defn pick-by-residue [residue pdb-data]
  (pick-by :resid residue pdb-data))

(defn pick-by-record [record pdb-data]
  (pick-by :record record pdb-data))

(defn pick-by-occupancy [occupancy pdb-data]
  (pick-by :occupancy occupancy pdb-data))

(defn pick-by-occupancy-range [start end pdb-data]
  (pick-by-range :occupancy start end pdb-data))

(defn pick-by-tempfact [temp-fact pdb-data]
  (pick-by :temp-fact temp-fact pdb-data))

(defn pick-by-tempfact-range [start end pdb-data]
  (pick-by-range :temp-fact start end pdb-data))

(defn pick-by-xrange [start end pdb-data]
  (pick-by-range :x start end pdb-data))

(defn pick-by-yrange [start end pdb-data]
  (pick-by-range :y start end pdb-data))

(defn pick-by-zrange [start end pdb-data]
  (pick-by-range :z start end pdb-data))

(defn pick-by-x [x pdb-data]
  (pick-by :x x pdb-data))

(defn pick-by-y [y pdb-data]
  (pick-by :y y pdb-data))

(defn pick-by-z [z pdb-data]
  (pick-by :z z pdb-data))

(defn pick-by-x-min [pdb-data]
  (first (sort-by-x pdb-data)))

(defn pick-by-x-max [pdb-data]
  (last (sort-by-x pdb-data)))

(defn pick-by-y-min [pdb-data]
  (first (sort-by-y pdb-data)))

(defn pick-by-y-max [pdb-data]
  (last (sort-by-y pdb-data)))

(defn pick-by-z-min [pdb-data]
  (first (sort-by-z pdb-data)))

(defn pick-by-z-max [pdb-data]
  (last (sort-by-z pdb-data)))

(defn calc-x-max [pdb-data]
  (last (map-keylook-x (sort-by-x pdb-data))))

(defn calc-x-min [pdb-data]
  (first (map-keylook-x (sort-by-x pdb-data))))

(defn calc-y-max [pdb-data]
  (last (map-keylook-x (sort-by-y pdb-data))))

(defn calc-y-min [pdb-data]
  (first (map-keylook-y (sort-by-y pdb-data))))

(defn calc-z-max [pdb-data]
  (last (map-keylook-z (sort-by-z pdb-data))))

(defn calc-z-min [pdb-data]
  (first (map-keylook-z (sort-by-z pdb-data))))

(defn pick-by-bounding-box-center [pdb-data]
  ())

(defn half-count [collection]
  (/ (count collection) 2.0))

(defn get-all-coords-w-serial [pdb-data]
  (mapv #(hash-map :serial (:serial %)
                   :x      (:x %)
                   :y      (:y %)
                   :z      (:z %)) pdb-data))

(defn mean [data-sequence]
  (incanter-stats/mean data-sequence))

(defn median [data-sequence]
  (incanter-stats/median data-sequence))

(defn calc-x-median [pdb-data]
  (median (drop-nil (map-keylook-x pdb-data))))

(defn calc-x-mean [pdb-data]
  (mean (drop-nil (map-keylook-x pdb-data))))

(defn calc-y-mean [pdb-data]
  (mean (drop-nil (map-keylook-y pdb-data))))

(defn calc-z-mean [pdb-data]
  (mean (drop-nil (map-keylook-z pdb-data))))

(defn amino-acids-w-seqnum [pdb-data]
  (into []
   (distinct
    (mapv
     #(hash-map
       :seqnum (:seqnum %) :resid (:resid %)) pdb-data))))

(defn amino-acid-frequency [pdb-data]
  (frequencies (map #(:resid %) (amino-acids-w-seqnum pdb-data))))

(defn amino-acid-percentage [pdb-data]
      ())

(defn amino-acid-frequency-by-region [pdb-data]
  ())

(defn prep-amino-acid-freq-bchart [pdb-data]
  (vector (vec (keys (amino-acid-frequency pdb-data)))
          (vec (vals (amino-acid-frequency pdb-data)))))

(defmacro name-realized [pdb]
  `(quote ~pdb))

(defn bar-chart-amino-acid-freq [pdb-data]
  (icore/view
   (charts/bar-chart
    (first (prep-amino-acid-freq-bchart pdb-data))
    (last  (prep-amino-acid-freq-bchart pdb-data))
    :title (str "Amino Acid frequency in "  " protein.")
    :y-label "Frequency"
    :x-label "Amino Acid")))

(defn group-by-seqnum [pdb-data]
  (sort (group-by :seqnum pdb-data)))

(defn group-by-chain [pdb-data]
  (drop-if nil (group-by :chain pdb-data)))

;;;;(ns lh.prototype.geocompose
  ;;;;(:require [clojure.math.numeric-tower :as math]))

(defn cube [edge]
  {:solid "cube" :edge edge})

(defn surface-area-cube [cube]
  (* (math/expt (:edge cube) 2) 6))

(defn volume-cube [cube]
  (math/expt (:edge cube) 3))

(defn vertices-cube [cube]
  ())

(defn edges-cube [cube]
  ())

(defn faces-cube [cube]
  ())

(defn cube-face-center-points [cube]
  ())

(defn cube-self-referential-front [cube]
  ())

(defn cube-self-referential-back [cube]
  ())

(defn cube-self-referential-up [cube]
  ())

(defn cube-self-referential-down [cube]
  ())

(defn cube-self-referntial-east [cube]
  ())

(defn cube-self-referential-west [cube]
  ())

(defn abstract-compose-cubes [compcube-geometry-struct cube]
  (if (= compcube-geometry-struct [])
    (vector cube)
    (vector cube compcube-geometry-struct)))


;;;; (ns lh.prototype.pdb-download
  ;;;; (:require [clojure.java.io :refer [input-stream output-stream copy]]))

(def pdb-base-url-chunk "http://www.rcsb.org/pdb/files/")

(def directory "pdb/")

(defn copy-pdb-file! [pdb]
  (with-open [in (input-stream (str pdb-base-url-chunk pdb ".pdb"))
              out (output-stream (str directory pdb ".pdb"))]
    (copy in out)))

(defn copy-mult-pdb! [pdb-sequence]
 (if (first pdb-sequence)
   (cons (copy-pdb-file! (first pdb-sequence))
         (copy-mult-pdb! (rest pdb-sequence)))
   (list)))
