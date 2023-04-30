(ns germ.models
  (:require
   [clojure.string]
   [clojure.test :as test]))

;; removes forwards slashes from front and end
(defn regex->string [regex]
  (str (subs (str regex) 1 (- (count (str regex)) 1))))

(def ref-regexes
  {:range (re-pattern "[A-Z]+[0-9]+:[A-Z]+[0-9]+")
   :cell  (re-pattern "[A-Z]+[0-9]+")})

(def ref-regex
  (re-pattern (clojure.string/join "|" (map regex->string (vals ref-regexes)))))

(test/is (= (re-matches ref-regex "A1") "A1"))
(test/is (= (re-matches ref-regex "AAB1") "AAB1"))
(test/is (= (re-matches ref-regex "B100") "B100"))
(test/is (= (re-matches ref-regex "A1!") nil))
(test/is (= (re-matches ref-regex "A1:A2") "A1:A2"))
(test/is (= (re-matches ref-regex "A1:B100") "A1:B100"))
(test/is (= (re-matches ref-regex "A1A1:B100") nil))
(test/is (= (re-matches ref-regex "A1:A1:B100") nil))
(test/is (= (re-matches ref-regex ":B100:A1") nil))

(defn coordinate [row col]
  {:row row :col col})

(defn ref->coordinate
  [ref]
  ;; returns [x y]
  (let [[_ col row] (re-matches #"([A-Z]+)([0-9]+)" ref)
        col-index   (dec (reduce (fn [acc c] (+ (* 26 acc) (- (.charCodeAt c) 64))) 0 col))]
    (coordinate (dec (js/parseInt row)) col-index)))

(test/is (= (ref->coordinate "A1") {:row 0, :col 0}))
(test/is (= (ref->coordinate "A100") {:row 99, :col 0}))
(test/is (= (ref->coordinate "AB50") {:row 49, :col 27}))

;; (defn generate-cell-id []
;;   (->
;;    ((.-random js/Math))
;;    (.toString 16)
;;    (.slice 2)))

(defn new-cell
  [{:keys [val loc sheet-id]
    :or {val      nil
         loc      (coordinate nil nil)
         sheet-id nil}}]
  {:val val
   :loc loc
   :sheet-id sheet-id}
   ;; color, background, border, colspan, rowspan, alignment
   ;; font, font size
   ;; bold/italic – support rich content?
   ;; :refers-to: list of cell IDs
   ;; :referred-from: list of cell IDs
   ;; :prescribes {}
   ;; :prescribed {} 
  #_(defn prescribes []
      {:value ""
       :to "by-cell"
       :description ""
       :proof []})

  #_(defn prescribed []
      {:value ""
       :by "by-cell"
       :description ""
       :proof-chain []}))

(defn new-sheet
  [id num-rows num-cols]
  {:id id
   :num-rows num-rows
   :num-cols num-cols})

(defn add-sheet-to-workbook 
  [workbook sheet]
  (assoc-in workbook [:sheets-index (:id sheet)] sheet))

(defn new-workbook
  [default-sheet-id default-rows default-cols]
  (add-sheet-to-workbook 
   {:sheets-index {}
    :cells-index {}}
   (new-sheet default-sheet-id default-rows default-cols)))

;; to store all cells under a workbook
(defn cells-index-key
  [sheet-id row col]
  [sheet-id (coordinate row col)])

(defn set-cell-properties
  [workbook sheet-id row col properties]
  ;; todo: shouldn't be able to set a cell outside the sheet's bounds
  (let [cell-index (cells-index-key sheet-id row col)
        existing-cell (get-in workbook [:cells-index cell-index])]
    (assoc-in workbook
              [:cells-index (cells-index-key sheet-id row col)]
              (if existing-cell
                (merge existing-cell properties)
                (merge (new-cell {:loc (coordinate row col)
                                  :sheet-id sheet-id})
                       properties)))))

(defn set-cell-val
  [workbook sheet-id row col val]
  (set-cell workbook sheet-id row col (new-cell {:val val})))

(defn get-cell
  [workbook sheet-id row col]
  (get (:cells-index workbook) (cells-index-key sheet-id row col)))

(defn get-cell-val
  [workbook sheet-id row col]
  (:val (get-cell workbook sheet-id row col)))

(test/is (= (set-cell-val (new-workbook "sheet1" 10 10) "sheet1" 1 1 "A1")
            {:sheets-index {"sheet1" {:id "sheet1", :num-rows 10, :num-cols 10}} ,
             :cells-index {["sheet1" {:row 1, :col 1}] {:val "A1", :loc {:row 1, :col 1}, :sheet-id "sheet1"}}}))
(test/is (= (get-cell-val (set-cell-val (new-workbook "sheet1" 10 10) "sheet1" 1 1 "A1") "sheet1" 1 1) "A1"))
(test/is (= (get-cell-val (set-cell-val (new-workbook "sheet1" 10 10) "sheet1" 1 1 "A1") "sheet1" 2 2) nil))

;; for computing vals
(defn cells-with-refs 
  [workbook]
  (->> workbook
       :cells-index
    ;;    remove str from here
       (filter (fn [[_ cell]] (re-matches ref-regex (str (:val cell)))))
       vals))

(test/is (= (cells-with-refs (set-cell-val (new-workbook "sheet1" 3 3) "sheet1" 1 1 "A1"))
            [{:val "A1", :loc {:row 1, :col 1}, :sheet-id "sheet1"}]))

;; for displaying sheet as a 2d matrix
(defn sheet->val-matrix
  [workbook sheet-id]
  (let [sheet (get-in workbook [:sheets-index sheet-id])]
    (vec (for [row (range (:num-rows sheet))]
           (vec (for [col (range (:num-cols sheet))]
                  (get-cell-val workbook sheet-id row col)))))))

;; for helper in tests 
(defn val-matrix->sheet [])

(test/is (= (sheet->val-matrix (set-cell-val (new-workbook "sheet1" 3 3) "sheet1" 1 1 "A1") "sheet1") 
            [[nil nil nil] [nil "A1" nil] [nil nil nil]]))
(test/is (= (sheet->val-matrix (set-cell-val (new-workbook "sheet1" 1 1) "sheet1" 0 0 "A1") "sheet1") 
            [["A1"]]))
(test/testing "for zero row size"
  (test/is (= (sheet->val-matrix (set-cell-val (new-workbook "sheet1" 0 1) "sheet1" 0 0 "A1") "sheet1") 
              [])))
(test/testing "with data out of bounds"
  (test/is (= (sheet->val-matrix (set-cell-val (new-workbook "sheet1" 4 4) "sheet1" 10 10 "A1") "sheet1")
              [[nil nil nil nil] [nil nil nil nil] [nil nil nil nil] [nil nil nil nil]]))
  (test/is (= (sheet->val-matrix (set-cell-val (new-workbook "sheet1" 4 4) "sheet1" -1 -1 "A1") "sheet1")
              [[nil nil nil nil] [nil nil nil nil] [nil nil nil nil] [nil nil nil nil]])))
