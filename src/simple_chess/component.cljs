(ns simple-chess.component
  (:require
   [clojure.string :as str]
   [re-frame.core :as rf]
   [simple-chess.board-util :as util]
   [simple-chess.event :as event]
   [simple-chess.piece :as piece]
   [simple-chess.sub :as sub]))

(def icons
  {{:color :white :type :rook}   piece/rook-white
   {:color :white :type :knight} piece/knight-white
   {:color :white :type :bishop} piece/bishop-white
   {:color :white :type :queen}  piece/queen-white
   {:color :white :type :king}   piece/king-white
   {:color :white :type :pawn}   piece/pawn-white
   {:color :black :type :pawn}   piece/pawn-black
   {:color :black :type :rook}   piece/rook-black
   {:color :black :type :knight} piece/knight-black
   {:color :black :type :bishop} piece/bishop-black
   {:color :black :type :queen}  piece/queen-black
   {:color :black :type :king}   piece/king-black})

(defn square-colors
  [file rank highlighted?]
  (cond
    highlighted?                   ["bg-yellow-300" "text-[#769656]"]
    (util/square-black? file rank) ["bg-[#769656]" "text-[#efeed3]"]
    :else                          ["bg-[#efeed3]" "text-[#769656]"]))

(defn square-attrs
  [attrs-base pos colors]
  (-> attrs-base
      (assoc :id pos
             :on-click (fn [_] (rf/dispatch [::event/select-square pos])))
      (update :class into colors)))

(defn square
  [attrs file rank]
  (let [pos           (util/coords->pos file rank)
        piece         @(rf/subscribe [::sub/piece pos])
        icon          (get icons piece)
        highlighted?  @(rf/subscribe [::sub/highlighted-square? pos])
        colors        (square-colors file rank highlighted?)
        updated-attrs (square-attrs attrs pos colors)]
    [:div updated-attrs
     (when icon [icon])
     (when (= rank 1)
       [:span
        {:class ["font-mono" "font-extrabold" "absolute" "bottom-0" "right-1"]}
        (str/lower-case (util/file->str file))])
     (when (= file 1)
       [:span
        {:class ["font-mono" "font-extrabold" "absolute" "top-0" "left-1"]}
        rank])]))

(defn board
  []
  (let [side @(rf/subscribe [::sub/side])]
    [:div.flex.justify-center.p-4
     [:div.grid.grid-cols-8.gap-0.border.border-black
      (for [rank (util/ranks side)
            file (util/files side)]
        ^{:key {:rank rank :file file}}
        [square {:class ["flex-none" "h-24" "w-24" "relative"]} file rank])]]))
