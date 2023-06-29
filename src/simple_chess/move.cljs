(ns simple-chess.move
  (:require [simple-chess.board-util :as util]))

(defn empty-squares-in-direction
  [direction pieces pos]
  (->> pos
       util/pos->coords
       (iterate #(map + % direction))
       (drop 1)
       (take-while #(and (apply util/valid-coords? %) (apply util/empty-square? pieces %)))
       (mapv util/coords->pos)))

(defn attacked-squares
  [pieces pos direction]
  (let [color         (get-in pieces [pos :color])
        empty-squares (empty-squares-in-direction direction pieces pos)
        attacked-pos  (->> pos
                           (or (last empty-squares))
                           util/pos->coords
                           (map + direction)
                           util/coords->pos
                           (util/opponent-square? color pieces))]
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
  [pieces pos]
  (let [coords          (util/pos->coords pos)
        color           (get-in pieces [pos :color])
        direction       (pawn-movement-direction color)
        max-distance    (if (pawn-in-starting-pos? color coords) 2 1)
        forward-moves   (->> (empty-squares-in-direction direction pieces pos)
                                (take max-distance)
                                (into #{}))
        attacking-moves (->> [[1 0] [-1 0]]
                             (map #(map + coords direction %))
                             (map util/coords->pos)
                             (map (partial util/opponent-square? color pieces))
                             (remove nil?))]
    (into forward-moves attacking-moves)))

(defn knight-moves
  [pieces pos]
  (let [moves  [[-2 -1] [-2 1] [-1 -2] [-1 2] [1 -2] [1 2] [2 -1] [2 1]]
        color  (get-in pieces [pos :color])
        coords (util/pos->coords pos)]
    (->> moves
         (mapv #(map + % coords))
         (map util/coords->pos)
         (remove nil?)
         (remove (partial util/allied-square? color pieces))
         (into #{}))))

(defn long-distance-piece
  [pieces pos directions]
  (->> directions
       (mapcat (partial attacked-squares pieces pos))
       (into #{})))

(defn rook-moves
  [pieces pos]
  (long-distance-piece pieces pos [[0 1] [0 -1] [1 0] [-1 0]]))

(defn bishop-moves
  [pieces pos]
  (long-distance-piece pieces pos [[1 1] [1 -1] [-1 1] [-1 -1]]))

(defn queen-moves
  [pieces pos]
  (long-distance-piece pieces pos [[1 1] [1 -1] [-1 1] [-1 -1] [0 1] [0 -1] [1 0] [-1 0]]))

(defn king-moves
  [pieces pos]
  (let [color  (get-in pieces [pos :color])
        coords (util/pos->coords pos)]
    (->> [[1 1] [1 -1] [-1 1] [-1 -1] [0 1] [0 -1] [1 0] [-1 0]]
         (map #(map + coords %))
         (map util/coords->pos)
         (remove nil?)
         (remove (partial util/allied-square? color pieces))
         (into #{}))))

(defn valid-move?
  [pieces from to]
  (let [piece (get pieces from)]
    (cond
      (= (:type piece) :pawn)   ((pawn-moves pieces from) to)
      (= (:type piece) :knight) ((knight-moves pieces from) to)
      (= (:type piece) :bishop) ((bishop-moves pieces from) to)
      (= (:type piece) :rook)   ((rook-moves pieces from) to)
      (= (:type piece) :queen)  ((queen-moves pieces from) to)
      (= (:type piece) :king)   ((king-moves pieces from) to)
      :else                     true)))
