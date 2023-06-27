(ns simple-chess.sub
  (:require [re-frame.core :as rf]))

(rf/reg-sub
  ::pieces
  (fn [db _]
    (:pieces db)))

(rf/reg-sub
  ::piece
  :<- [::pieces]
  (fn [pieces [_ pos]]
    (get pieces pos)))

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
