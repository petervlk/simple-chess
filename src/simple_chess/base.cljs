(ns simple-chess.base
  (:require
   [simple-chess.board-util :as util]
   [simple-chess.sub :as sub]
   [simple-chess.event :as event]
   [re-frame.core :as rf]))

(defn square-colors
  [file rank highlighted]
  (cond
    (highlighted (util/coords->pos file rank)) ["bg-yellow-300"]
    (util/square-black? file rank)             ["bg-[#769656]"]
    :else                                      ["bg-[#efeed3]"]))

(defn square-attrs
  [attrs-base colors]
  (update attrs-base :class into colors))

(defn square
  [attrs file rank piece highlighted]
  (let [colors        (square-colors file rank highlighted)
        updated-attrs (square-attrs attrs colors)]
    [:div updated-attrs
     (when piece ((:icon-fn piece)))]))

(defn board
  []
  (let [pieces      @(rf/subscribe [::sub/pieces])
        side        @(rf/subscribe [::sub/side])
        highlighted @(rf/subscribe [::sub/highlighted-squares])]
    [:div.flex.justify-center.p-4
     [:div.grid.grid-cols-8.gap-0.border.border-black
      (for [rank (util/ranks side)
            file (util/files side)
            :let [pos (util/coords->pos file rank)
                  piece (get pieces pos)]]
        ^{:key pos}
        [square
         {:id       pos
          :class    ["flex-none" "h-24" "w-24"]
          :on-click (fn [_] (rf/dispatch [::event/select-square pos]))}
         file
         rank
         piece
         highlighted])]]))
