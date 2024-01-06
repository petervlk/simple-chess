(ns simple-chess.validity
  (:require
   [simple-chess.constants]
   [simple-chess.move :as m]
   [simple-chess.board-util :as util]))

(defn attacked-squares
  [board attacking-color]
  (->> board
       (filter (fn [[_ {:keys [color]}]] (= color attacking-color)))
       (mapcat
         (fn [[pos {piece-type :type}]]
           (case piece-type
             :pawn   (m/pawn-moves-attacking board pos)
             :knight (m/knight-moves board pos)
             :bishop (m/bishop-moves board pos)
             :rook   (m/rook-moves board pos)
             :queen  (m/queen-moves board pos)
             :king   (m/king-moves-basic board pos))))
       (into #{})))

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
  ([color board]
   (pieces [:king :queen :rook :bishop :knight :pawn] color board))
  ([types color board]
   (->> board
        (filter
          (fn [[_pos {piece-type :type piece-color :color}]]
            (and (= color piece-color) (some #(= piece-type %) types))))
        (mapv first))))

(defn in-check?  [board color-to-move]
  (let [squares-under-attack (attacked-squares board (util/opponent-color color-to-move))
        king-pos             (first (pieces [:king] color-to-move board))]
    (some #(= king-pos %) squares-under-attack)))

(defn available-moves
  [board color-to-move moves]
  (mapcat (partial piece-moves board moves) (pieces color-to-move board)))

(defn game-state
  [board color-to-move moves]
  (let [check?                 (in-check? board color-to-move)
        avail-moves            (available-moves board color-to-move moves)
        check-preventing-move? (comp
                                 not
                                 #(in-check? % color-to-move)
                                 (partial apply m/board-after-move board))]
    (cond
      (and (not check?) (empty? avail-moves))                           :stale-mate
      (and check? (empty? (filter check-preventing-move? avail-moves))) :check-mate
      :else                                                             :game-in-progress)))

(defn valid-move?
  [board moves from to]
  (let [color (get-in board [from :color])]
    (and
      ((piece-moves board moves from) [from to])
      (not (in-check? (m/board-after-move board from to) color)))))
