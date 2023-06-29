(ns simple-chess.board-util)

(def board-dimension 8)
(def board-start-idx 1)

(def board-range-asc (range board-start-idx (inc board-dimension)))
(def board-range-desc (range board-dimension (dec board-start-idx) -1))

(defn in-board-range
  [n]
  (<= board-start-idx n board-dimension))

(defn valid-coords?
  [file rank]
  (and
    (in-board-range file)
    (in-board-range rank)
    [file rank]))

(defn ranks
  [side]
  (if (= side :white)
    board-range-desc
    board-range-asc))

(defn files
  [side]
  (if (= side :white)
    board-range-asc
    board-range-desc))

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
