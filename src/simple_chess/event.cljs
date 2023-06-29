(ns simple-chess.event
  (:require
   [re-frame.core :as rf]
   [simple-chess.move :as move]))

(def new-game-state
  {:moves  []
   :turn   :white
   :side   :white
   :pieces {"A1" {:color :white :type :rook}
            "B1" {:color :white :type :knight}
            "C1" {:color :white :type :bishop}
            "D1" {:color :white :type :queen}
            "E1" {:color :white :type :king}
            "F1" {:color :white :type :bishop}
            "G1" {:color :white :type :knight}
            "H1" {:color :white :type :rook}
            "A2" {:color :white :type :pawn}
            "B2" {:color :white :type :pawn}
            "C2" {:color :white :type :pawn}
            "D2" {:color :white :type :pawn}
            "E2" {:color :white :type :pawn}
            "F2" {:color :white :type :pawn}
            "G2" {:color :white :type :pawn}
            "H2" {:color :white :type :pawn}
            "A7" {:color :black :type :pawn}
            "B7" {:color :black :type :pawn}
            "C7" {:color :black :type :pawn}
            "D7" {:color :black :type :pawn}
            "E7" {:color :black :type :pawn}
            "F7" {:color :black :type :pawn}
            "G7" {:color :black :type :pawn}
            "H7" {:color :black :type :pawn}
            "A8" {:color :black :type :rook}
            "B8" {:color :black :type :knight}
            "C8" {:color :black :type :bishop}
            "D8" {:color :black :type :queen}
            "E8" {:color :black :type :king}
            "F8" {:color :black :type :bishop}
            "G8" {:color :black :type :knight}
            "H8" {:color :black :type :rook}}})

(rf/reg-event-db
  ::initialize
  (fn [_ _]
    new-game-state))

(defn move-piece [{:keys [db]} [_ from to]]
  (when (move/valid-move? (:pieces db) from to)
    (let [piece (get-in db [:pieces from])]
      {:db (-> db
               (assoc-in [:pieces to] piece)
               (update :pieces dissoc from))
       :fx [[:dispatch [::change-turn]]
            [:dispatch [::log-move from to]]]})))

(rf/reg-event-fx
  ::move
  move-piece)

(defn select-square
  [{:keys [db]} [_ pos]]
  (let [selected                 (:selected db)
        piece-color              (fn [pos] (get-in db [:pieces pos :color]))
        picking-opponents-piece? (fn [pos] (and (not selected)
                                                (not= (:turn db) (piece-color pos))))
        picking-own-piece?       (fn [pos] (and (not selected)
                                                (= (:turn db) (piece-color pos))))
        unpicking?               (fn [pos] (and selected (= selected pos)))
        repicking-piece?         (fn [pos] (and selected
                                                (not= selected pos)
                                                (= (piece-color selected) (piece-color pos))))]
    (cond
      (picking-opponents-piece? pos) nil
      (picking-own-piece? pos)       {:db (assoc db :selected pos)}
      (unpicking? pos)               {:db (dissoc db :selected)}
      (repicking-piece? pos)         {:db (assoc db :selected pos)}
      :else                          {:fx [[:dispatch [::move selected pos]]]})))

(rf/reg-event-fx
  ::select-square
  select-square)

(rf/reg-event-db
  ::change-turn
  (fn [db _]
    (let [new-color (if (= (:turn db) :white)
                      :black
                      :white)]
      (-> db
          (assoc :turn new-color)
          (dissoc :selected)))))

(rf/reg-event-db
  ::log-move
  (fn [db [_ from to]]
    (update db :moves (fnil conj []) [from to])))
