(ns simple-chess.event
  (:require
   [re-frame.core :as rf]
   [simple-chess.piece :as piece]))

(def new-game-state
  {:side   :white
   :pieces {"A1" {:color "white" :type "rook" :icon-fn piece/rook-white}
            "B1" {:color "white" :type "knight" :icon-fn piece/knight-white}
            "C1" {:color "white" :type "bishop" :icon-fn piece/bishop-white}
            "D1" {:color "white" :type "queen" :icon-fn piece/queen-white}
            "E1" {:color "white" :type "king" :icon-fn piece/king-white}
            "F1" {:color "white" :type "bishop" :icon-fn piece/bishop-white}
            "G1" {:color "white" :type "knight" :icon-fn piece/knight-white}
            "H1" {:color "white" :type "rook" :icon-fn piece/rook-white}
            "A2" {:color "white" :type "pawn" :icon-fn piece/pawn-white}
            "B2" {:color "white" :type "pawn" :icon-fn piece/pawn-white}
            "C2" {:color "white" :type "pawn" :icon-fn piece/pawn-white}
            "D2" {:color "white" :type "pawn" :icon-fn piece/pawn-white}
            "E2" {:color "white" :type "pawn" :icon-fn piece/pawn-white}
            "F2" {:color "white" :type "pawn" :icon-fn piece/pawn-white}
            "G2" {:color "white" :type "pawn" :icon-fn piece/pawn-white}
            "H2" {:color "white" :type "pawn" :icon-fn piece/pawn-white}
            "A7" {:color "black" :type "pawn" :icon-fn piece/pawn-black}
            "B7" {:color "black" :type "pawn" :icon-fn piece/pawn-black}
            "C7" {:color "black" :type "pawn" :icon-fn piece/pawn-black}
            "D7" {:color "black" :type "pawn" :icon-fn piece/pawn-black}
            "E7" {:color "black" :type "pawn" :icon-fn piece/pawn-black}
            "F7" {:color "black" :type "pawn" :icon-fn piece/pawn-black}
            "G7" {:color "black" :type "pawn" :icon-fn piece/pawn-black}
            "H7" {:color "black" :type "pawn" :icon-fn piece/pawn-black}
            "A8" {:color "black" :type "rook" :icon-fn piece/rook-black}
            "B8" {:color "black" :type "knight" :icon-fn piece/knight-black}
            "C8" {:color "black" :type "bishop" :icon-fn piece/bishop-black}
            "D8" {:color "black" :type "queen" :icon-fn piece/queen-black}
            "E8" {:color "black" :type "king" :icon-fn piece/king-black}
            "F8" {:color "black" :type "bishop" :icon-fn piece/bishop-black}
            "G8" {:color "black" :type "knight" :icon-fn piece/knight-black}
            "H8" {:color "black" :type "rook" :icon-fn piece/rook-black}}})

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

(defn same-color?
  [db pos1 pos2]
  (->> [pos1 pos2]
       (map #(get-in db [:pieces % :color]))
       (apply =)))

(defn select-square
  [{:keys [db]} [_ pos]]
  (let [selected (:selected db)]
    (cond
      (and (nil? selected) (get-in db [:pieces pos])) {:db (assoc db :selected pos)}
      (and selected (= selected pos))                 {:db (dissoc db :selected)}
      (and selected
           (not= selected pos)
           (same-color? db selected pos))             {:db (assoc db :selected pos)}
      (and selected
           (not= selected pos)
           (not (same-color? db selected pos)))       {:fx [[:dispatch [::move selected pos]]]
                                                       :db (dissoc db :selected)})))

(rf/reg-event-fx
  ::select-square
  select-square)
