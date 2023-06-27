(ns simple-chess.board-util)

(def board-dimension 8)
(def board-start-idx 1)

(def board-range-asc (range board-start-idx (inc board-dimension)))
(def board-range-desc (range board-dimension (dec board-start-idx) -1))

(defn in-board-range
  [n]
  (<= board-start-idx n board-dimension))

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

(defn coords->pos
  [file rank]
  (when (and (in-board-range file) (in-board-range rank))
    (-> \A
       (.charCodeAt 0)
       (+ file)
       dec
       char
       (str rank))))

(defn pos->coords
  [pos]
  (vector
    (inc (- (.charCodeAt pos 0) (.charCodeAt \A 0)))
    (inc (- (.charCodeAt pos 1) (.charCodeAt \1 0)))))

(defn square-black?
  [file rank]
  (odd? (+ (dec file) rank)))