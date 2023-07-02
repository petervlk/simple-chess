(ns simple-chess.special-move
  (:require [simple-chess.board-util :as util]))

(defn en-passant-possible?
  [board moves pos]
  (let [white-move?       (= :white (get-in board [pos :color]))
        prev-move         (last moves)
        prev-move-end-pos (second prev-move)]
    (when (and
            (= :pawn (get-in board [prev-move-end-pos :type]))
            (= (mapv util/position-rank prev-move) (if white-move? [7 5] [2 4]))
            (= (util/position-rank pos) (util/position-rank prev-move-end-pos))
            (= 1 (Math/abs (- (util/position-file pos) (util/position-file prev-move-end-pos)))))
      (hash-set (str (first prev-move-end-pos) (if white-move? 6 3))))))

(defn castling-path-clear?
  [board color castling-side]
  (let [rank  (if (= color :white) 1 8)
        files (if (= castling-side :king-side) ["F" "G"] ["B" "C" "D"])]
    (->> files
         (mapv #(str % rank))
         (every? #(nil? (get board %))))))

(defn castled-king-pos
  [color castling-side]
  (let [rank (if (= color :white) 1 8)
        file (if (= castling-side :king-side) "G" "C")]
    (str file rank)))

(defn king-starting-pos
  [color]
  (if (= color :white) "E1" "E8"))

(defn rook-starting-pos
  [color side]
  (let [rank (if (= color :white) 1 8)
        file (if (= side :king-side) "H" "A")]
    (str file rank)))

(defn castling-possible?
  [board moves pos castling-side]
  (let [unmoved-piece? (fn [moves square] (empty? (filter #(= square (first %)) moves)))
        color          (get-in board [pos :color])
        king-init-pos  (king-starting-pos color)
        rook-init-pos  (rook-starting-pos color castling-side)]
    (and (unmoved-piece? moves king-init-pos)
         (unmoved-piece? moves rook-init-pos)
         (castling-path-clear? board color castling-side)
         (castled-king-pos color castling-side))))

(defn castling-moves
  [board moves pos]
  (->> [:king-side :queen-side]
       (map (partial castling-possible? board moves pos))
       (remove nil?)))
