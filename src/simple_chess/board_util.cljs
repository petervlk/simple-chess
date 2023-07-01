(ns simple-chess.board-util
  (:require [simple-chess.constants :as consts]))

(defn in-board-range
  [n]
  (<= consts/board-start-idx n consts/board-dimension))

(defn valid-coords?
  [file rank]
  (and
    (in-board-range file)
    (in-board-range rank)
    [file rank]))

(defn ranks
  [side]
  (if (= side :white)
    consts/board-range-desc
    consts/board-range-asc))

(defn files
  [side]
  (if (= side :white)
    consts/board-range-asc
    consts/board-range-desc))

(defn file->str
  [file]
  (-> \A
       (.charCodeAt 0)
       (+ file)
       dec
       char))

(defn coords->pos
  ([[file rank]]
   (coords->pos file rank))
  ([file rank]
   (when (valid-coords? file rank)
     (str (file->str file) rank))))

(defn pos->coords
  [pos]
  (vector
    (inc (- (.charCodeAt pos 0) (.charCodeAt \A 0)))
    (inc (- (.charCodeAt pos 1) (.charCodeAt \1 0)))))

(defn square-black?
  [file rank]
  (odd? (+ (dec file) rank)))

(defn square-piece
  [board file rank]
  (get board (coords->pos file rank)))

(defn empty-square?
  [board file rank]
  (nil? (square-piece board file rank)))

(defn allied-square?
  [color board pos]
  (and (= color (:color (get board pos))) pos))

(defn opponent-square?
  [color board pos]
  (let [piece (get board pos)]
    (and piece (not= color (:color piece)) pos)))

(defn promotion-rank?
  [pos]
  (-> pos
      second
      #{"1" "8"}))

(defn en-passant-pos
  [from to]
  (str (first to) (second from)))

(defn castling-rook-move
  [king-pos]
  (let [king-file (first king-pos)
        rank (second king-pos)]
    (if (= king-file "G")
      (mapv #(str % rank) ["H" "F"])
      (mapv #(str % rank) ["A" "D"]))))

(defn king-castling-move?
  [from to]
  (#{["E1" "G1"]
     ["E1" "C1"]
     ["E8" "G8"]
     ["E8" "C8"]}
    (vector from to)))
