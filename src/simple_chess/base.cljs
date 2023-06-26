(ns simple-chess.base
  (:require
   [simple-chess.sub :as sub]
   [simple-chess.event :as event]
   [re-frame.core :as rf]))

(def board-dimension 8)
(def board-start-idx 1)

(def board-range-asc (range board-start-idx (inc board-dimension)))
(def board-range-desc (range board-dimension (dec board-start-idx) -1))

(defn ranks
  [side]
  (if (= side :white)
    board-range-desc
    board-range-asc))

(defn files
  [side]
  (if (= side :white)
    board-range-asc
    board-range-desc))

(defn coords->pos
  [file rank]
  (-> \A
      (.charCodeAt 0)
      (+ file)
      dec
      char
      (str rank)))

(defn pos->coords
  [pos]
  (vector
    (inc (- (.charCodeAt pos 0) (.charCodeAt \A 0)))
    (inc (- (.charCodeAt pos 1) (.charCodeAt \1 0)))))

(defn square-black?
  [file rank]
  (odd? (+ (dec file) rank)))

(defn square-colors
  [black?]
  (if black?
    ["bg-[#769656]" "text-gray-400"]
    ["bg-[#efeed3]" "text-gray-400"]))

(defn square-attrs
  [attrs-base colors]
  (update attrs-base :class into colors))

(defn square
  [attrs file rank piece]
  (let [black?        (square-black? file rank)
        colors        (square-colors black?)
        updated-attrs (square-attrs attrs colors)]
    [:div updated-attrs
     (when piece (piece))]))

(defn board
  []
  (let [pieces @(rf/subscribe [::sub/pieces])
        side   @(rf/subscribe [::sub/side])]
    [:div.flex.justify-center.p-4
     [:div.grid.grid-cols-8.gap-0.border.border-black
      (for [rank (ranks side)
            file (files side)
            :let [pos (coords->pos file rank)
                  piece (get pieces pos)]]
        ^{:key pos}
        [square
         {:id    pos
          :class ["flex-none" "h-24" "w-24" "hover:bg-green-500/50"]
          :on-click (fn [_] (rf/dispatch [::event/select-square pos]))}
         file
         rank
         piece])]]))
