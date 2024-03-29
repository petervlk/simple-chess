(ns simple-chess.app
  (:require
   [reagent.dom :as reagent-dom]
   [re-frame.core :as rf]
   [simple-chess.component :as component]
   [simple-chess.event :as event]))

(defn ^:dev/after-load render
  "Render the toplevel component for this app."
  []
  (let [root-el (.getElementById js/document "app")]
    (reagent-dom/unmount-component-at-node root-el)
    (reagent-dom/render [component/board] root-el)))

(defn ^:export init
  "Run application startup logic."
  []
  (rf/dispatch-sync [::event/initialize])
  (render))
