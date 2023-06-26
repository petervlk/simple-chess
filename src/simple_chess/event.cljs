(ns simple-chess.event
  (:require
   [re-frame.core :as rf]
   [simple-chess.piece :as piece]))

(def new-game-state
  {:side   :white
   :pieces {"A1" piece/rook-white
            "B1" piece/knight-white
            "C1" piece/bishop-white
            "D1" piece/queen-white
            "E1" piece/king-white
            "F1" piece/bishop-white
            "G1" piece/knight-white
            "H1" piece/rook-white
            "A2" piece/pawn-white
            "B2" piece/pawn-white
            "C2" piece/pawn-white
            "D2" piece/pawn-white
            "E2" piece/pawn-white
            "F2" piece/pawn-white
            "G2" piece/pawn-white
            "H2" piece/pawn-white
            "A7" piece/pawn-black
            "B7" piece/pawn-black
            "C7" piece/pawn-black
            "D7" piece/pawn-black
            "E7" piece/pawn-black
            "F7" piece/pawn-black
            "G7" piece/pawn-black
            "H7" piece/pawn-black
            "A8" piece/rook-black
            "B8" piece/knight-black
            "C8" piece/bishop-black
            "D8" piece/queen-black
            "E8" piece/king-black
            "F8" piece/bishop-black
            "G8" piece/knight-black
            "H8" piece/rook-black}})

(rf/reg-event-db
  ::initialize
  (fn [_ _]
    new-game-state))

(defn move-piece [db [_ from to]]
    (let [piece (get-in db [:pieces from])]
      (-> db
          (assoc-in [:pieces to] piece)
          (update :pieces dissoc from))))

(rf/reg-event-db
  ::move
  move-piece)

(defn select-square
  [{:keys [db]} [_ pos]]
  (let [selected (:selected db)]
    (cond
      (nil? selected)                    {:db (assoc db :selected pos)}
      (and selected (not= selected pos)) {:fx [[:dispatch [::move selected pos]]]
                                          :db (dissoc db :selected)}
      :else                              nil))
  )

(rf/reg-event-fx
  ::select-square
  select-square)

(comment
  (rf/dispatch-sync [::initialize])
  (rf/dispatch [::move "D2" "D4"])
  (rf/dispatch [::move "C1" "F4"])
  (rf/dispatch [::move "F4" "C7"])
  (rf/dispatch [::move "C7" "D8"])
  (rf/dispatch [::move "C7" "D8"])
  (rf/dispatch [::move "E1" "E8"])
  )
