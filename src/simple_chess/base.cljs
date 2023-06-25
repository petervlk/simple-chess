(ns simple-chess.base
  (:require
   [simple-chess.piece :as piece]
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

(def new-game-state
  {:side   :white
   :pieces {"A1" piece/rook-white
            "B1" piece/knight-white
            "C1" piece/bishop-white
            "D1" piece/queen-white
            "E1" piece/king-white
            "F1" piece/bishop-white
            "G1" piece/knight-white
            "H1" piece/rook-white
            "A2" piece/pawn-white
            "B2" piece/pawn-white
            "C2" piece/pawn-white
            "D2" piece/pawn-white
            "E2" piece/pawn-white
            "F2" piece/pawn-white
            "G2" piece/pawn-white
            "H2" piece/pawn-white
            "A7" piece/pawn-black
            "B7" piece/pawn-black
            "C7" piece/pawn-black
            "D7" piece/pawn-black
            "E7" piece/pawn-black
            "F7" piece/pawn-black
            "G7" piece/pawn-black
            "H7" piece/pawn-black
            "A8" piece/rook-black
            "B8" piece/knight-black
            "C8" piece/bishop-black
            "D8" piece/queen-black
            "E8" piece/king-black
            "F8" piece/bishop-black
            "G8" piece/knight-black
            "H8" piece/rook-black}})

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

(rf/reg-sub
  :pieces
  (fn [db _]
    (:pieces db)))

(rf/reg-sub
  :side
  (fn [db _]
    (:side db)))

(defn board
  []
  (let [pieces @(rf/subscribe [:pieces])
        side   @(rf/subscribe [:side])]
    [:div.flex.justify-center.p-4
     [:div.grid.grid-cols-8.gap-0.border.border-black
      (for [rank (ranks side)
            file (files side)
            :let [pos (coords->pos file rank)
                  piece (get pieces pos)]]
        ^{:key pos}
        [square
         {:id    pos
          :class ["flex-none" "h-24" "w-24" "hover:bg-green-500/50"]}
         file
         rank
         piece])]]))
