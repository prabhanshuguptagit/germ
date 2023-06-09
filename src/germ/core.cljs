(ns germ.core
  (:require [sci.core :as sci]
            [clojure.string]
            [clojure.test :as test]))

(def sci-opts {:namespaces {'clojure.string {'split clojure.string/split}}})
(def sci-ctx (sci/init sci-opts))

(defn empty-grid
  ([rows cols]
   (empty-grid rows cols nil))
  ([rows cols default-value]
   (vec (repeat rows (vec (repeat cols default-value))))))

(defonce init-grid
  (atom (empty-grid 40 20)))

(defonce calculated-grid
  (atom @init-grid))

#_(atom
   [['(inc B3) 2                '(inc A1)]
    [4         '(map inc A1:A3)  6]
    [7         8                 9]])

  
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
          (mapv #(evaluate-cell grid %) result))))))

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
        (clojure.string/replace (str expr)
                                ref-regex
                                #(let [evaled-ref (eval-ref grid %1)]
                                   (if (string? evaled-ref)
                                    ;; hack for raw strings in cells.
                                    ;; if we evaluate a cell that returns a raw string,
                                    ;; its wrapped in additional string quotes so that
                                    ;; it's not eval-ed as a symbol.
                                    ;; This belongs somewhere else probably, in evaluate-cell
                                    ;; or eval-ref
                                     (str "\"" evaled-ref "\"")
                                     (str evaled-ref))))]
    (if (re-find ref-regex replaced-expr)
      (replace-refs-with-values grid replaced-expr)
      replaced-expr)))

(defn normalize-cell-value
  [cell-value-str]
  (cond
    (re-matches #"^\d+$" cell-value-str) cell-value-str
    (re-matches ref-regex cell-value-str) cell-value-str
    (contains? #{"\"" "#" "'" "(" "[" "{"} (first cell-value-str)) cell-value-str
    :else (str "\"" cell-value-str "\"")))

(defn evaluate-cell
  [grid cell-value]
  (when-not (nil? cell-value)
    (sci/eval-string*
      sci-ctx
      (replace-refs-with-values grid (normalize-cell-value cell-value)))))

(defn formulas [grid]
  (->> grid
       (map-indexed (fn [row-index row]
                      (map-indexed (fn [column-index cell]
                                     [[row-index column-index]
                                      ((fnil re-matches ref-regex "") ref-regex cell)])
                                   row)))
       (apply concat)
       (remove (comp nil? second))
       (into {})))

(defn num-rows [grid]
  (count grid))

(defn num-cols [grid]
  (count (first grid)))

(defn all-cell-coordinates [grid]
  (for [x (range (num-rows grid))
        y (range (num-cols grid))]
    [x y]))

(defn spill-sequence
  [grid coordinates sequence-value]
  (->> (map vector
            (repeat coordinates)
            (range (count sequence-value)))
       (map (fn [[coord thing-to-add]]
              [(+ thing-to-add (first coord))
               (second coord)]))
       (map vector sequence-value)
       (reduce (fn [new-grid [cell-value coords]]
                 (assoc-in new-grid coords (str cell-value)))
               grid)))

(defn spill-all-sequences [grid]
  (reduce (fn [the-grid [row col]]
            (if (sequential? (get-cell the-grid row col))
              (spill-sequence the-grid [row col] (get-cell the-grid row col))
              the-grid))
          grid
          (all-cell-coordinates grid)))

(defn apply-formulas [grid formulas]
  (->> (reduce (fn [the-grid [cell-coordinate formula-string]]
                 (assoc-in the-grid cell-coordinate (evaluate-cell grid formula-string)))
               grid
               formulas)
       (spill-all-sequences)))

(defn converge-formulas [grid formulas]
  (let [new-grid (apply-formulas grid formulas)]
    (if (= grid new-grid)
      new-grid
      (recur new-grid formulas))))

(defn evaluate-grid
  [grid]
  (mapv (fn [row] (mapv #(evaluate-cell grid %) row)) grid))

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
   "<div class='small-row-marker'>🦠</div>"
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
                                      "onkeydown='germ.core.handle_cell_input(event)'"
                                      "onfocus='germ.core.show_cell_raw_value(event)'"
                                      "onfocusout='germ.core.show_cell_calculated_value(event)'"
                                      ">"))
                               row))
                   "</div>"))
                grid))
    "</div>")))

;; recalculating first the cells with no inputs
;; then recalculating any cell whose inputs are ready
(defn ^:dev/after-load init
  ([] (init 0 0))
  ([focus-row-index focus-col-index]
   (reset! calculated-grid (evaluate-grid @init-grid))
   (set! (.-innerHTML (.getElementById js/document "root"))
         (html @calculated-grid))
   (when-let [next-input (js/document.querySelector
                          (str "[data-row-index='" focus-row-index  "'][data-col-index='" focus-col-index "']"))]
     (.focus next-input))))


(defn handle-cell-input
  [event]

  (when (= "Escape" (.-key event))
    (.blur (.-target event)))

  (when (contains? #{"ArrowUp" "ArrowDown"} (.-key event))
    (let [row-index (js/parseInt (aget (.-dataset (.-target event)) "rowIndex"))
          col-index (js/parseInt (aget (.-dataset (.-target event)) "colIndex"))]
      (case (.-key event)
        "ArrowUp" (init (max 0 (dec row-index)) col-index)
        "ArrowDown" (init (inc row-index) col-index))))

  (when (contains? #{"Tab" "Enter"} (.-key event))
    (.preventDefault event)
    (let [row-index (js/parseInt (aget (.-dataset (.-target event)) "rowIndex"))
          col-index (js/parseInt (aget (.-dataset (.-target event)) "colIndex"))
          value (.-value (.-target event))]
      (reset! init-grid (set-cell @init-grid row-index col-index value))
      (if (= (.-key event) "Tab")
        (if (.-shiftKey event)
          (init row-index (max 0 (dec col-index)))
          (init row-index (inc col-index)))
        (init (inc row-index) col-index)))))

(defn show-cell-raw-value
  [event]
  (let [row-index (js/parseInt (aget (.-dataset (.-target event)) "rowIndex"))
        col-index (js/parseInt (aget (.-dataset (.-target event)) "colIndex"))]
    (set! (.. event -target -value) (get-in @init-grid [row-index col-index]))))

(defn show-cell-calculated-value
  [event]
  (let [row-index (js/parseInt (aget (.-dataset (.-target event)) "rowIndex"))
        col-index (js/parseInt (aget (.-dataset (.-target event)) "colIndex"))]
    (set! (.. event -target -value) (get-in @calculated-grid [row-index col-index]))))