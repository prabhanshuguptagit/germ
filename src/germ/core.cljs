(ns germ.core
  (:require [sci.core :as sci]
            [clojure.string]
            [clojure.test :as test]))

(defn empty-grid
  ([rows cols]
   (empty-grid rows cols nil))
  ([rows cols default-value]
   (vec (repeat rows (vec (repeat cols default-value))))))

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
        col-index (reduce (fn [acc c] (+ (* 26 acc) (- (.charCodeAt c) 64))) 0 col)]
    [(dec (js/parseInt row)) (dec col-index)]))

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

(defn resolve-ref
  [grid ref]
  (let [single-cell-regex #"[A-Z]+[0-9]+"
        range-regex #"[A-Z]+[0-9]+:[A-Z]+[0-9]+"
        auto-expand-regex #"[A-Z]+[0-9]+::"]
    (cond 
      (re-matches single-cell-regex ref)
      (let [[row col] (cell-coords ref)]
        (get-cell grid row col))

      (re-matches range-regex ref)
      (let [[start end] (map cell-coords (re-seq single-cell-regex ref))]
        (sub-grid grid start end))

      ;; (re-matches auto-expand-regex ref)
      ;; (let [[start] (cell-coords (re-matches single-cell-regex ref))
      ;;       [start-row start-col] start]
      ;;   (sub-grid grid start end))
      )))

(def dummy-grid
  [[1 2 3]
   [4 5 6]
   [7 8 9]])
(def grid
  [['(inc B3) 2                '(inc A1)]
   [4         '(map inc A1:A3)  6]
   [7         8                 9]])

(def grid-2
  [['(inc C3) '(mapv inc A1:A3) nil]
   [4         nil              nil]
   [7         nil              8]])

(test/is (= (resolve-ref dummy-grid "A1") 1))
(test/is (= (resolve-ref dummy-grid "B3") 8))
(test/is (= (resolve-ref dummy-grid "A1:A2") [1 4]))
(test/is (= (resolve-ref dummy-grid "A1:A1") [1]))
(test/is (= (resolve-ref dummy-grid "A1:B1") [[1 2]]))
(test/is (= (resolve-ref dummy-grid "A1:C1") [[1 2 3]]))
(test/is (= (resolve-ref dummy-grid "A1:B2") [[1 2] [4 5]]))
(test/is (= (resolve-ref dummy-grid "B2:C3") [[5 6] [8 9]]))
(test/is (= (resolve-ref dummy-grid "A1:C3") [[1 2 3]
                                              [4 5 6]
                                              [7 8 9]]))

(def ref-regex #"[A-Z]+[0-9]+:[A-Z]+[0-9]+|[A-Z]+[0-9]+")

;; (defmacro replace-refs-with-values [grid expr]
;;   (cond
;;     (list? expr) (quote `(map (fn [x]
;;                                (if (re-matches ref-regex (str x)) 
;;                                  (resolve-ref [[1 2 3]
;;                                                [4 5 6]
;;                                                [7 8 9]] (str x))
;;                                  x)) ~expr))
;;     (re-matches ref-regex (str expr)) (resolve-ref grid expr)
;;     :else (do
;;             (println "dwdwedioh")
;;             expr)))


;; todo: could do a macro
;; - read-string happens before substitution, nice place to catch errors
;; - don't have to string replace
(defn replace-refs-with-values
  [grid expr]
  (let [replaced-expr
        (clojure.string/replace (str expr) ref-regex #(str (resolve-ref grid %1)))]
    (if (re-find ref-regex (str replaced-expr))
      (replace-refs-with-values grid replaced-expr)
      replaced-expr)))

(defn evaluate-cell
  [grid cell-value]
  (if-not (nil? cell-value)
    (sci/eval-string
     (replace-refs-with-values grid cell-value))))

(defn spill-cells
  [grid]
  ;; is iseq, map do |cell_value| 
  ;; if cell_value is iseq spill-row over each column
  )

(defn evaluate-grid
  [grid]
  (map (fn [row] (map #(evaluate-cell grid %) row)) grid))

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

;; recalculating first the cells with no inputs
;; then recalculating any cell whose inputs are ready
(defn ^:dev/after-load init []
  (set! (.-innerHTML (.getElementById js/document "root")) 
        (str 
         "original grid"
         "<br>"
         (print-grid grid)
         "<br>"
         "calculated grid"
             "<br>"
             (print-grid (evaluate-grid grid)))))

