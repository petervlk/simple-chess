(ns simple-chess.board-util
  (:require [simple-chess.constants :as consts]))

(defn in-board-range
  [n]
  (<= consts/board-start-idx n consts/board-dimension))

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
  [file rank]
  (when (and (in-board-range file) (in-board-range rank))
    (str (file->str file) rank)))

(defn pos->coords
  [pos]
  (vector
    (inc (- (.charCodeAt pos 0) (.charCodeAt \A 0)))
    (inc (- (.charCodeAt pos 1) (.charCodeAt \1 0)))))

(defn- position-property
  [extractor pos]
  (when pos (extractor (pos->coords pos))))

(def position-rank (partial position-property second))
(def position-file (partial position-property first))

(defn position-moved
  [direction pos]
  (apply coords->pos (map + direction (pos->coords pos))))

(defn square-black?
  [pos]
  (odd? (+ (dec (position-file pos)) (position-rank pos))))

(defn piece-color
  [board pos]
  (let [piece (get board pos)]
    (and piece (:color piece))))

(defn empty-square?
  [board pos]
  (nil? (get board pos)))

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

(defn opponent-color
  [color]
  (if (= color :white)
    :black
    :white))
