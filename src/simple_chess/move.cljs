(ns simple-chess.move
  (:require [simple-chess.board-util :as util]))

(def knight-jumps [[-2 -1] [-2 1] [-1 -2] [-1 2] [1 -2] [1 2] [2 -1] [2 1]])
(def rook-directions [[0 1] [0 -1] [1 0] [-1 0]])
(def bishop-directions [[1 1] [1 -1] [-1 1] [-1 -1]])
(def all-directions (into rook-directions bishop-directions))

(defn empty-squares-in-direction
  [direction board pos]
  (->> pos
       util/pos->coords
       (iterate #(map + % direction))
       (drop 1)
       (take-while #(and (apply util/valid-coords? %) (apply util/empty-square? board %)))
       (mapv util/coords->pos)))

(defn attacked-squares
  [board pos direction]
  (let [color         (get-in board [pos :color])
        empty-squares (empty-squares-in-direction direction board pos)
        attacked-pos  (->> pos
                           (or (last empty-squares))
                           util/pos->coords
                           (map + direction)
                           util/coords->pos
                           (util/opponent-square? color board))]
    (if attacked-pos
      (conj empty-squares attacked-pos)
      empty-squares)))

(defn pawn-in-starting-pos?
  [color [_file rank]]
  (or (and (= color :white) (= rank 2))
      (and (= color :black) (= rank (dec util/board-dimension)))))

(defn pawn-movement-direction
  [color]
  (if (= color :white) [0 1] [0 -1]))

(defn pawn-moves
  [board pos]
  (let [coords          (util/pos->coords pos)
        color           (get-in board [pos :color])
        direction       (pawn-movement-direction color)
        max-distance    (if (pawn-in-starting-pos? color coords) 2 1)
        forward-moves   (->> (empty-squares-in-direction direction board pos)
                             (take max-distance)
                             (into #{}))
        attacking-moves (->> [[1 0] [-1 0]]
                             (map #(map + coords direction %))
                             (map util/coords->pos)
                             (map (partial util/opponent-square? color board))
                             (remove nil?))]
    (into forward-moves attacking-moves)))

(defn knight-moves
  [board pos]
  (let [color  (get-in board [pos :color])
        coords (util/pos->coords pos)]
    (->> knight-jumps
         (mapv #(map + % coords))
         (map util/coords->pos)
         (remove nil?)
         (remove (partial util/allied-square? color board))
         (into #{}))))

(defn long-distance-moves
  [directions]
  (fn [board pos]
    (->> directions
         (mapcat (partial attacked-squares board pos))
         (into #{}))))

(def rook-moves (long-distance-moves rook-directions))
(def bishop-moves (long-distance-moves bishop-directions))
(def queen-moves (long-distance-moves all-directions))

(defn king-moves
  [board pos]
  (let [color  (get-in board [pos :color])
        coords (util/pos->coords pos)]
    (->> all-directions
         (map #(map + coords %))
         (map util/coords->pos)
         (remove nil?)
         (remove (partial util/allied-square? color board))
         (into #{}))))

(defn valid-move?
  [board from to]
  (case (get-in board [from :type])
    :pawn   ((pawn-moves board from) to)
    :knight ((knight-moves board from) to)
    :bishop ((bishop-moves board from) to)
    :rook   ((rook-moves board from) to)
    :queen  ((queen-moves board from) to)
    :king   ((king-moves board from) to)))
