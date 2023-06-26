(ns simple-chess.sub
  (:require [re-frame.core :as rf]))

(rf/reg-sub
  ::pieces
  (fn [db _]
    (:pieces db)))

(rf/reg-sub
  ::side
  (fn [db _]
    (:side db)))
