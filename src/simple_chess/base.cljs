(ns simple-chess.base
  (:require
   [simple-chess.piece :as piece]))

(defn square-label
  [file rank]
  (-> \A
      (.charCodeAt 0)
      (+ file)
      char
      (str rank)))

(defn square-black?
  [file rank]
  (even? (+ (dec file) rank)))

(defn square-colors
  [black?]
  (if black?
    ["bg-[#769656]" "text-gray-400"]
    ["bg-[#efeed3]" "text-gray-400"]))

(defn square-attrs
  [attrs-base colors]
  (update attrs-base :class into colors))

(defn square
  [attrs file rank]
  (let [black? (square-black? file rank)
        colors (square-colors black?)
        updated-attrs (square-attrs attrs colors)]
    [:div updated-attrs
     (if black?
       [piece/knight-white]
       [piece/rook-black])]))

(defn board
  []
  [:div.flex.justify-center.p-4
   [:div.grid.grid-cols-8.gap-0.border.border-black
    (for [rank (range 8 0 -1)
          file (range 8)]
      ^{:key {:r rank :f file}}
      [square
       {:class ["flex-none" "h-24" "w-24" "hover:bg-green-500/50"]}
       file
       rank])]])
