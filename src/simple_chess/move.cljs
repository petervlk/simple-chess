(ns simple-chess.move
  (:require [simple-chess.board-util :as util]))

(defn square-piece
  [pieces file rank]
  (get pieces (util/coords->pos file rank)))

(defn empty-square
  [pieces file rank]
  (nil? (square-piece pieces file rank)))

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
                                (take-while (partial empty-square pieces file))
                                (take (if starting-position? 2 1))
                                (map #(util/coords->pos file %))
                                (into #{}))]
    (cond-> forward-moves
      (opponents-piece (inc file) (step rank)) (conj (util/coords->pos (inc file) (step rank)))
      (opponents-piece (dec file) (step rank)) (conj (util/coords->pos (dec file) (step rank))))))

(defn valid-move?
  [pieces from to]
  (let [piece (get pieces from)]
    (cond
      (= :pawn (:type piece)) ((pawn-moves pieces from) to)
      :else                   true)))
