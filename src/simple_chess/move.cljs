(ns simple-chess.move
  (:require[simple-chess.board-util :as util]
           [simple-chess.constants :as const]))

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
      (and (= color :black) (= rank (dec const/board-dimension)))))

(defn pawn-movement-direction
  [color]
  (if (= color :white) [0 1] [0 -1]))

(defn en-passant-possible?
  [board moves pos]
  (let [white-move?       (= :white (get-in board [pos :color]))
        prev-move         (last moves)
        prev-move-end-pos (second prev-move)
        prev-move-coords  (map util/pos->coords prev-move)
        prev-move-file    (ffirst prev-move-coords)
        coords            (util/pos->coords pos)]
    (and (= :pawn (get-in board [prev-move-end-pos :type]))
         (= (mapv second prev-move-coords) (if white-move? [7 5] [2 4]))
         (= 1 (Math/abs (- (first coords) prev-move-file)))
         (if white-move? [prev-move-file 6] [prev-move-file 3]))))

(defn pawn-moves
  [board moves pos]
  (let [coords          (util/pos->coords pos)
        color           (get-in board [pos :color])
        direction       (pawn-movement-direction color)
        max-distance    (if (pawn-in-starting-pos? color coords) 2 1)
        en-passant-move (en-passant-possible? board moves pos)
        forward-moves   (take max-distance (empty-squares-in-direction direction board pos))
        attacking-moves (->> [[1 0] [-1 0]]
                             (map #(map + coords direction %))
                             (map util/coords->pos)
                             (map (partial util/opponent-square? color board))
                             (remove nil?))]
    (cond-> #{}
      forward-moves   (into forward-moves)
      attacking-moves (into attacking-moves)
      en-passant-move (into (vector (util/coords->pos en-passant-move))))))

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

(defn king-starting-pos
  [color]
  (if (= color :white) "E1" "E8"))

(defn rook-starting-pos
  [color side]
  (let [rank (if (= color :white) 1 8)
        file (if (= side :king-side) "H" "A")]
    (str file rank)))

(defn castling-path-clear?
  [board color castling-side]
  (let [rank (if (= color :white) 1 8)
        files (if (= castling-side :king-side) ["F" "G"] ["B" "C" "D"])]
    (->> files
         (mapv #(str % rank))
         (every? #(nil? (get board %))))))

(defn castled-king-pos
  [color castling-side]
  (let [rank (if (= color :white) 1 8)
        file (if (= castling-side :king-side) "G" "C")]
    (str file rank)))

(defn castling-possible?
  [board moves pos castling-side]
  (let [square-without-moves? (fn [moves square] (empty? (filter #(= square (first %)) moves)))
        color                 (get-in board [pos :color])
        king-init-pos         (king-starting-pos color)
        rook-init-pos         (rook-starting-pos color castling-side)]
    (and (square-without-moves? moves king-init-pos)
         (square-without-moves? moves rook-init-pos)
         (castling-path-clear? board color castling-side)
         (castled-king-pos color castling-side))))

(defn king-moves
  [board moves pos]
  (let [color          (get-in board [pos :color])
        coords         (util/pos->coords pos)
        standard-moves (->> all-directions
                            (map #(map + coords %))
                            (map util/coords->pos)
                            (remove nil?)
                            (remove (partial util/allied-square? color board)))
        castling       (->> [:king-side :queen-side]
                            (map (partial castling-possible? board moves pos))
                            (remove nil?))]
    (cond-> #{}
      (seq standard-moves) (into standard-moves)
      (seq castling)       (into castling))))

(defn board-after-move
  [board from to]
  (let [moved-piece (get board from)]
    (-> board
        (assoc to moved-piece)
        (dissoc from))))
