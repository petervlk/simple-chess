(ns simple-chess.app
  (:require
   [reagent.dom :as reagent-dom]
   [simple-chess.base :as base]))

(defn ^:dev/after-load render
  "Render the toplevel component for this app."
  []
  (let [root-el (.getElementById js/document "app")]
    (reagent-dom/unmount-component-at-node root-el)
    (reagent-dom/render [base/board] root-el)))

(defn ^:export init
  "Run application startup logic."
  []
  (render))
