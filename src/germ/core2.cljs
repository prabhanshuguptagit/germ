(ns germ.core2
  (:require [sci.core :as sci]
            [clojure.string]
            [germ.models :as models]
            [reagent.dom :as rdom]))

(def sci-opts {:namespaces {'clojure.string {'split clojure.string/split}}})
(def sci-ctx (sci/init sci-opts))
(defonce current-sheet-id "sheet-1")
(defonce workbook (atom (models/new-workbook current-sheet-id 10 10)))
(swap! workbook models/set-cell-val current-sheet-id 1 1 "A1")
(swap! workbook models/set-cell-val current-sheet-id 2 2 "B100")
(defn show [] (models/sheet->val-matrix @workbook current-sheet-id))

(declare handle-cell-input)
(defn html
  [grid]
  [:div {:class :sheet}
   [:div {:class :small-row-marker} "🦠"]
   (map-indexed
    (fn [col-index _]
      [:div {:class :col-marker
             :key   (str "col-marker-" col-index)}
       (models/column-number->reference col-index)])
    (first grid))
   (map-indexed
    (fn [row-index row]
      [:div {:class :row
             :key   (str "row-" row-index)}
       [:div {:class :row-marker} (inc row-index)]
       (map-indexed
        (fn [col-index cell]
          [:input {:key            (str "cell-input-" row-index "-" col-index)
                   :data-row-index row-index
                   :data-col-index col-index
                   :class          :cell
                   :type           :text
                   :defaultValue   cell
                   :onKeyDown      #(handle-cell-input %)}])
        row)])
    grid)])

(defn cell-input-dom [row col]
  (js/document.querySelector
   (str "[data-row-index='" row  "'][data-col-index='" col "']")))

(defn render-sheet
  ([] (render-sheet 0 0))
  ([focus-row focus-col]
   (rdom/render (html (show)) (.getElementById js/document "root"))
   (when-let [next-input (cell-input-dom focus-row focus-col)]
     (.focus next-input))))

(defn handle-cell-input
  [event]
  (when (= "Escape" (.-key event))
    (.blur (.-target event)))

  (when (contains? #{"ArrowUp" "ArrowDown"} (.-key event))
    (let [row-index (js/parseInt (aget (.-dataset (.-target event)) "rowIndex"))
          col-index (js/parseInt (aget (.-dataset (.-target event)) "colIndex"))]
      (case (.-key event)
        "ArrowUp" (render-sheet (max 0 (dec row-index)) col-index)
        "ArrowDown" (render-sheet (inc row-index) col-index))))

  (when (contains? #{"Tab" "Enter"} (.-key event))
    (.preventDefault event)
    (let [row-index (js/parseInt (aget (.-dataset (.-target event)) "rowIndex"))
          col-index (js/parseInt (aget (.-dataset (.-target event)) "colIndex"))
          value (.-value (.-target event))]
      (swap! workbook models/set-cell-val current-sheet-id row-index col-index value)
      (if (= (.-key event) "Tab")
        (if (.-shiftKey event)
          (render-sheet row-index (max 0 (dec col-index)))
          (render-sheet row-index (inc col-index)))
        (render-sheet (inc row-index) col-index)))))

(defn ^:dev/after-load init
  []
  (render-sheet))
