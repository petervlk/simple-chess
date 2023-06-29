(ns simple-chess.constants)

(def board-dimension 8)
(def board-start-idx 1)

(def board-range-asc (range board-start-idx (inc board-dimension)))
(def board-range-desc (range board-dimension (dec board-start-idx) -1))

(def rook-white-attrs   {:color :white :type :rook})
(def knight-white-attrs {:color :white :type :knight})
(def bishop-white-attrs {:color :white :type :bishop})
(def queen-white-attrs  {:color :white :type :queen})
(def king-white-attrs   {:color :white :type :king})
(def pawn-white-attrs   {:color :white :type :pawn})
(def pawn-black-attrs   {:color :black :type :pawn})
(def rook-black-attrs   {:color :black :type :rook})
(def knight-black-attrs {:color :black :type :knight})
(def bishop-black-attrs {:color :black :type :bishop})
(def queen-black-attrs  {:color :black :type :queen})
(def king-black-attrs   {:color :black :type :king})

(def initial-board-config
  {"A1" rook-white-attrs
   "B1" knight-white-attrs
   "C1" bishop-white-attrs
   "D1" queen-white-attrs
   "E1" king-white-attrs
   "F1" bishop-white-attrs
   "G1" knight-white-attrs
   "H1" rook-white-attrs
   "A2" pawn-white-attrs
   "B2" pawn-white-attrs
   "C2" pawn-white-attrs
   "D2" pawn-white-attrs
   "E2" pawn-white-attrs
   "F2" pawn-white-attrs
   "G2" pawn-white-attrs
   "H2" pawn-white-attrs
   "A7" pawn-black-attrs
   "B7" pawn-black-attrs
   "C7" pawn-black-attrs
   "D7" pawn-black-attrs
   "E7" pawn-black-attrs
   "F7" pawn-black-attrs
   "G7" pawn-black-attrs
   "H7" pawn-black-attrs
   "A8" rook-black-attrs
   "B8" knight-black-attrs
   "C8" bishop-black-attrs
   "D8" queen-black-attrs
   "E8" king-black-attrs
   "F8" bishop-black-attrs
   "G8" knight-black-attrs
   "H8" rook-black-attrs})