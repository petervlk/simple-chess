(ns simple-chess.sub
  (:require [re-frame.core :as rf]))

(rf/reg-sub
  ::board
  (fn [db _]
    (:board db)))

(rf/reg-sub
  ::piece
  :<- [::board]
  (fn [board [_ pos]]
    (get board pos)))

(rf/reg-sub
  ::side
  (fn [db _]
    (:side db)))

(rf/reg-sub
  ::selected
  (fn [db _]
    (:selected db)))

(rf/reg-sub
  ::moves
  (fn [db _]
    (:moves db)))

(rf/reg-sub
  ::highlighted-square?
  :<- [::selected]
  :<- [::moves]
  (fn [[selected moves] [_ pos]]
    (or (= pos selected) (some #(= pos %) (last moves)))))
