(ns simple-chess.event
  (:require
   [re-frame.core :as rf]
   [simple-chess.piece :as piece]))

(def new-game-state
  {:moves  []
   :turn   :white
   :side   :white
   :pieces {"A1" {:color :white :type :rook :icon-fn piece/rook-white}
            "B1" {:color :white :type :knight :icon-fn piece/knight-white}
            "C1" {:color :white :type :bishop :icon-fn piece/bishop-white}
            "D1" {:color :white :type :queen :icon-fn piece/queen-white}
            "E1" {:color :white :type :king :icon-fn piece/king-white}
            "F1" {:color :white :type :bishop :icon-fn piece/bishop-white}
            "G1" {:color :white :type :knight :icon-fn piece/knight-white}
            "H1" {:color :white :type :rook :icon-fn piece/rook-white}
            "A2" {:color :white :type :pawn :icon-fn piece/pawn-white}
            "B2" {:color :white :type :pawn :icon-fn piece/pawn-white}
            "C2" {:color :white :type :pawn :icon-fn piece/pawn-white}
            "D2" {:color :white :type :pawn :icon-fn piece/pawn-white}
            "E2" {:color :white :type :pawn :icon-fn piece/pawn-white}
            "F2" {:color :white :type :pawn :icon-fn piece/pawn-white}
            "G2" {:color :white :type :pawn :icon-fn piece/pawn-white}
            "H2" {:color :white :type :pawn :icon-fn piece/pawn-white}
            "A7" {:color :black :type :pawn :icon-fn piece/pawn-black}
            "B7" {:color :black :type :pawn :icon-fn piece/pawn-black}
            "C7" {:color :black :type :pawn :icon-fn piece/pawn-black}
            "D7" {:color :black :type :pawn :icon-fn piece/pawn-black}
            "E7" {:color :black :type :pawn :icon-fn piece/pawn-black}
            "F7" {:color :black :type :pawn :icon-fn piece/pawn-black}
            "G7" {:color :black :type :pawn :icon-fn piece/pawn-black}
            "H7" {:color :black :type :pawn :icon-fn piece/pawn-black}
            "A8" {:color :black :type :rook :icon-fn piece/rook-black}
            "B8" {:color :black :type :knight :icon-fn piece/knight-black}
            "C8" {:color :black :type :bishop :icon-fn piece/bishop-black}
            "D8" {:color :black :type :queen :icon-fn piece/queen-black}
            "E8" {:color :black :type :king :icon-fn piece/king-black}
            "F8" {:color :black :type :bishop :icon-fn piece/bishop-black}
            "G8" {:color :black :type :knight :icon-fn piece/knight-black}
            "H8" {:color :black :type :rook :icon-fn piece/rook-black}}})

(rf/reg-event-db
  ::initialize
  (fn [_ _]
    new-game-state))

(defn move-piece [{:keys [db]} [_ from to]]
  (let [piece (get-in db [:pieces from])]
    {:db (-> db
             (assoc-in [:pieces to] piece)
             (update :pieces dissoc from))
     :fx [[:dispatch [::change-turn]]
          [:dispatch [::log-move from to]]]}))

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
