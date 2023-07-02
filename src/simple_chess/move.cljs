(ns simple-chess.move
  (:require[simple-chess.board-util :as util]
           [simple-chess.constants :as const]
           [simple-chess.special-move :as special]))

(defn squares-in-direction
  [direction board pos include-opponent? limit]
  (let [color     (get-in board [pos :color])
        reduce-fn (fn [squares pos]
                    (cond
                      (nil? pos)                                    (reduced squares)
                      (util/empty-square? board pos)                (conj squares pos)
                      (and include-opponent?
                           (util/opponent-square? color board pos)) (reduced (conj squares pos))
                      :else                                         (reduced squares)))]
    (->> pos
         (iterate #(util/position-moved direction %))
         (drop 1)
         (reduce reduce-fn [])
         (take limit))))

(defn visible-squares
  ([directions]
   (visible-squares directions const/board-dimension))
  ([directions limit]
   (fn [board pos]
     (->> directions
          (mapcat #(squares-in-direction % board pos true limit))
          (into #{})))))

(def knight-jumps [[-2 -1] [-2 1] [-1 -2] [-1 2] [1 -2] [1 2] [2 -1] [2 1]])
(def rook-directions [[0 1] [0 -1] [1 0] [-1 0]])
(def bishop-directions [[1 1] [1 -1] [-1 1] [-1 -1]])
(def all-directions (into rook-directions bishop-directions))

(def rook-moves (visible-squares rook-directions))
(def bishop-moves (visible-squares bishop-directions))
(def queen-moves (visible-squares all-directions))
(def knight-moves (visible-squares knight-jumps 1))
(def king-moves-basic (visible-squares all-directions 1))

(defn king-moves
  [board moves pos]
  (into (king-moves-basic board pos) (special/castling-moves board moves pos)))

(defn pawn-in-starting-pos?
  [color pos]
  (let [rank (util/position-rank pos)]
    (or (and (= color :white) (= rank 2))
        (and (= color :black) (= rank (dec const/board-dimension))))))

(defn pawn-movement-direction
  [color]
  (if (= color :white) [0 1] [0 -1]))

(defn pawn-moves-basic
  [board pos]
  (let [color                (get-in board [pos :color])
        direction            (pawn-movement-direction color)
        limit-fwd            (if (pawn-in-starting-pos? color pos) 2 1)
        forward-moves        (squares-in-direction direction board pos false limit-fwd)
        attacking-directions (map #(map + direction %) [[1 0] [-1 0]])
        attacking-moves      (->> attacking-directions
                                  (map #(util/position-moved % pos))
                                  (map (partial util/opponent-square? color board))
                                  (remove nil?))]
    (into #{} (concat forward-moves attacking-moves))))

(defn pawn-moves
  [board moves pos]
  (into (pawn-moves-basic board pos) (special/en-passant-possible? board moves pos)))

(defn board-after-move
  [board from to]
  (let [moved-piece (get board from)]
    (-> board
        (assoc to moved-piece)
        (dissoc from))))
