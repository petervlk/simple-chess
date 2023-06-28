(ns simple-chess.move
  (:require [simple-chess.board-util :as util]))

(defn square-piece
  [pieces file rank]
  (get pieces (util/coords->pos file rank)))

(defn empty-square?
  [pieces file rank]
  (nil? (square-piece pieces file rank)))

(defn allied-square?
  [color pieces pos]
  (and (= color (:color (get pieces pos))) pos))

(defn opponent-square?
  [color pieces pos]
  (let [piece (get pieces pos)]
    (and piece (not= color (:color piece)) pos)))

(defn empty-squares-in-direction
  [direction pieces pos]
  (->> pos
       util/pos->coords
       (iterate #(map + % direction))
       (drop 1)
       (take-while #(and (apply util/valid-coords? %) (apply empty-square? pieces %)))
       (mapv (partial apply util/coords->pos))))

(defn attacked-squares
  [pieces pos direction]
  (let [color         (get-in pieces [pos :color])
        empty-squares (empty-squares-in-direction direction pieces pos)
        attacked-pos  (->> pos
                           (or (last empty-squares))
                           util/pos->coords
                           (map + direction)
                           (apply util/coords->pos)
                           (opponent-square? color pieces))]
    (if attacked-pos
      (conj empty-squares attacked-pos)
      empty-squares)))

(defn pawn-moves
  [pieces pos]
  (let [[file rank]        (util/pos->coords pos)
        color              (get-in pieces [pos :color])
        step               (if (= color :white) inc dec)
        starting-position? (or (and (= color :white) (= rank 2))
                               (and (= color :black) (= rank (dec util/board-dimension))))
        opponents-piece    (fn [file rank]
                             (let [piece (square-piece pieces file rank)]
                               (and piece (not= color (:color piece)))))
        forward-moves      (->> rank
                                (iterate step)
                                (drop 1)
                                (take-while (partial empty-square? pieces file))
                                (take (if starting-position? 2 1))
                                (map #(util/coords->pos file %))
                                (into #{}))]
    (cond-> forward-moves
      (opponents-piece (inc file) (step rank)) (conj (util/coords->pos (inc file) (step rank)))
      (opponents-piece (dec file) (step rank)) (conj (util/coords->pos (dec file) (step rank))))))

(defn knight-moves
  [pieces pos]
  (let [moves  [[-2 -1] [-2 1] [-1 -2] [-1 2] [1 -2] [1 2] [2 -1] [2 1]]
        color  (get-in pieces [pos :color])
        coords (util/pos->coords pos)]
    (->> moves
         (mapv #(map + % coords))
         (map (partial apply util/coords->pos))
         (remove nil?)
         (remove (partial allied-square? color pieces))
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

(defn valid-move?
  [pieces from to]
  (let [piece (get pieces from)]
    (cond
      (= (:type piece) :pawn )  ((pawn-moves pieces from) to)
      (= (:type piece) :knight) ((knight-moves pieces from) to)
      (= (:type piece) :rook)   ((rook-moves pieces from) to)
      (= (:type piece) :bishop) ((bishop-moves pieces from) to)
      (= (:type piece) :queen)  ((queen-moves pieces from) to)
      :else                     true)))
