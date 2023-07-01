(ns simple-chess.validity
  (:require
   [simple-chess.board-util :as util]
   [simple-chess.constants]
   [simple-chess.move :as m]))

(defn piece-moves
  [board moves pos]
  (->> (case (get-in board [pos :type])
         :pawn   (m/pawn-moves board moves pos)
         :knight (m/knight-moves board pos)
         :bishop (m/bishop-moves board pos)
         :rook   (m/rook-moves board pos)
         :queen  (m/queen-moves board pos)
         :king   (m/king-moves board moves pos))
       (map #(vector pos %))
       (into #{})))

(defn pieces
  [types color board]
  (->> board
       (filter
         (fn [[_pos {piece-type :type piece-color :color}]]
           (and (= color piece-color) (some #(= piece-type %) types))))
       (mapv first)))

(defn opponent-color
  [color]
  (if (= :white color) :black :white))

(defn in-check?
  [board color moves]
  (let [potential-checking-types [:queen :rook :bishop :knight :pawn]
        potential-attackers      (pieces potential-checking-types (opponent-color color) board)
        attacked-squares         (->> potential-attackers
                                      (mapcat #(piece-moves board moves %))
                                      (map second)
                                      (into #{}))
        king-pos                 (first (pieces [:king] color board))]
    (some #(= king-pos %) attacked-squares)))

(defn valid-move?
  [board moves from to]
  (let [color (get-in board [from :color])]
    (and
      ((piece-moves board moves from) [from to])
      (not (in-check? (m/board-after-move board from to) color [])))))

(comment
  (piece-moves simple-chess.constants/initial-board-config [] "C1")

  (valid-move? simple-chess.constants/initial-board-config [] "A2" "A4")
  (valid-move? simple-chess.constants/initial-board-config [] "A2" "A5")

  (in-check?
    (-> simple-chess.constants/initial-board-config
        (dissoc "E7")
        (assoc-in ["E2" :type] :queen))
    :black [])

  (pieces [:king] :white simple-chess.constants/initial-board-config)

  ,)
