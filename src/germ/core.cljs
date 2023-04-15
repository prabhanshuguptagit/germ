(ns germ.core
  (:require [sci.core :as sci]
            [clojure.string]
            [clojure.test :as test]))

(defn empty-grid
  ([rows cols]
   (empty-grid rows cols nil))
  ([rows cols default-value]
   (vec (repeat rows (vec (repeat cols default-value))))))

(defonce init-grid
  (atom (empty-grid 40 10))
  #_(atom
   [['(inc B3) 2                '(inc A1)]
    [4         '(map inc A1:A3)  6]
    [7         8                 9]]))

(defn column-number-to-reference [n]
  ;; this only does columns A to Z rn, should make it work for AA etc
  (char (+ 65 n)))

;; (def grid (empty-grid 10 10 1))

(defn set-cell
  [grid row col value]
  (assoc-in grid [row col] value))

(defn get-cell
  [grid row col]
  (get-in grid [row col]))

(defn cell-coords
  [ref]
  ;; returns [x y]
  (let [[_ col row] (re-matches #"([A-Z]+)([0-9]+)" ref)
        col-index (dec (reduce (fn [acc c] (+ (* 26 acc) (- (.charCodeAt c) 64))) 0 col))]
    [(dec (js/parseInt row)) col-index]))

(defn sub-grid-by-spaces
  [])

(defn sub-grid
  [grid [start-row start-col] [end-row end-col]]
  (let [result (->> (subvec grid start-row (inc end-row))
                    (mapv (fn [row] (subvec row start-col (inc end-col)))))]
    (if (= end-col start-col)
      (mapv first result)
      result)))

(defn take-while-not-nil [coll]
  (take-while identity coll))

(declare evaluate-cell)
(declare evaluate-grid)

(defn eval-ref
  [grid ref]
  (let [single-cell-regex #"[A-Z]+[0-9]+"
        range-regex #"[A-Z]+[0-9]+:[A-Z]+[0-9]+"
        auto-expand-regex #"[A-Z]+[0-9]+::"]
    (cond 
      (re-matches single-cell-regex ref)
      (let [[row col] (cell-coords ref)]
        (evaluate-cell grid (get-cell grid row col)))

      (re-matches range-regex ref)
      (let [[start end] (map cell-coords (re-seq single-cell-regex ref))
            result (sub-grid grid start end)]
        (cond
          (vector? (first result))
          (evaluate-grid result)
          :else
          (mapv #(evaluate-cell grid %) result)))

      ;; (re-matches auto-expand-regex ref)
      ;; (let [[start] (cell-coords (re-matches single-cell-regex ref))
      ;;       [start-row start-col] start]
      ;;   (sub-grid grid start end))
      )))

(def dummy-grid
  [["1" "2" "3"]
   ["4" "5" "6"]
   ["7" "8" "9"]])

(def grid-2
  [['(inc C3) '(mapv inc A1:A3) nil]
   [4         nil              nil]
   [7         nil              8]])

(test/is (= (eval-ref dummy-grid "A1") 1))
(test/is (= (eval-ref dummy-grid "B3") 8))
(test/is (= (eval-ref dummy-grid "A1:A2") [1 4]))
(test/is (= (eval-ref dummy-grid "A1:A1") [1]))
(test/is (= (eval-ref dummy-grid "A1:B1") [[1 2]]))
(test/is (= (eval-ref dummy-grid "A1:C1") [[1 2 3]]))
(test/is (= (eval-ref dummy-grid "A1:B2") [[1 2] [4 5]]))
(test/is (= (eval-ref dummy-grid "B2:C3") [[5 6] [8 9]]))
(test/is (= (eval-ref dummy-grid "A1:C3") [[1 2 3]
                                              [4 5 6]
                                              [7 8 9]]))

(def ref-regex #"[A-Z]+[0-9]+:[A-Z]+[0-9]+|[A-Z]+[0-9]+")

;; todo: could do a macro
;; - read-string happens before substitution, nice place to catch errors
;; - don't have to string replace
(defn replace-refs-with-values
  [grid expr]
  (let [replaced-expr
        (clojure.string/replace (str expr) ref-regex #(str (eval-ref grid %1)))]
    (if (re-find ref-regex (str replaced-expr))
      (replace-refs-with-values grid replaced-expr)
      replaced-expr)))

(defn normalize-cell-value
  [cell-value-str]
  (cond
    (re-find #"^\d+$" cell-value-str) cell-value-str
    (re-find ref-regex cell-value-str) cell-value-str
    (= (first cell-value-str) "(") cell-value-str
    :else (str "\"" cell-value-str "\"")))

(defn evaluate-cell
  [grid cell-value]
  (when-not (nil? cell-value)
    (sci/eval-string
     (replace-refs-with-values grid (normalize-cell-value cell-value)))))

(defn spill-cells
  [grid]
  ;; is iseq, map do |cell_value| 
  ;; if cell_value is iseq spill-row over each column
  )

(defn evaluate-grid
  [grid]
  (mapv (fn [row] (mapv #(evaluate-cell grid %) row)) grid))

(defn spill
  [grid])

(defn print-grid
  [grid]
  (let [formatted-grid (map
                        (fn [row]
                          (map
                           (fn [cell]
                             (let [cell-size 18
                                   cell-str (str cell)
                                   spaces (- cell-size (count cell-str))]
                               (str  " | " cell-str (apply str (repeat spaces " ")))))
                           row)) grid)]
    (reduce 
     (fn [acc row] (str (str acc (clojure.string/join row) " | ") "<br>"))
            ""
            formatted-grid)))

(defn html
  [grid]
  (str
   "<div class='sheet'>"
   "<div class='small-row-marker'>❤️</div>"
   (apply str (map-indexed (fn [col-index _]
                             (str "<div class='col-marker'>" (column-number-to-reference col-index) "</div>"))
                           (first grid)))
       (str
        (apply str (map-indexed
                    (fn [row-index row]
                      (str
                       "<div class='row'>"
                       "<div class='row-marker'>"
                       (inc row-index)
                       "</div>"
                       (apply str (map-indexed
                                   (fn [col-index cell]
                                     (str "<input data-row-index=" row-index
                                          " data-col-index=" col-index
                                          " class='cell' type='text' value="
                                          "'" (str cell) "'"
                                          "onkeydown='germ.core.update_cell(event)'"
                                          ">"))
                                   row))
                       "</div>"))
                    grid))
        "</div>")))

;; recalculating first the cells with no inputs
;; then recalculating any cell whose inputs are ready
(defn ^:dev/after-load init []
  (set! (.-innerHTML (.getElementById js/document "root")) 
        (html (evaluate-grid @init-grid))
        #_(str 
         "original grid"
         "<br>"
         (print-grid @init-grid)
         "<br>"
         "calculated grid"
             "<br>"
             (print-grid (evaluate-grid @init-grid)))))

(defn update-cell
  [event]
  (when (contains? #{"Tab" "Enter"} (.-key event))
    (.preventDefault event)
    (let [row-index (js/parseInt (aget (.-dataset (.-target event)) "rowIndex"))
          col-index (js/parseInt (aget (.-dataset (.-target event)) "colIndex"))
          value (.-value (.-target event))]
      (reset! init-grid (set-cell @init-grid row-index col-index value))
      (init))))
