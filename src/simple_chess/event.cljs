(ns simple-chess.event
  (:require
   [re-frame.core :as rf]
   [simple-chess.validity :as validity]
   [simple-chess.constants :refer [initial-board-config]]
   [simple-chess.board-util :as util]))

(def new-game-state
  {:moves  []
   :turn   :white
   :side   :white
   :board initial-board-config})

(rf/reg-event-db
  ::initialize
  (fn [_ _]
    new-game-state))

(defn move-piece [{:keys [db]} [_ from to]]
  (when (validity/valid-move? (:board db) (:moves db) from to)
    (let [piece       (get-in db [:board from])
          promotion?  (and (= :pawn (:type piece)) (util/promotion-rank? to))
          en-passant? (and (= :pawn (:type piece))
                           (not= (first from) (first to))
                           (nil? (get-in db [:board to])))
          castling?   (and (= :king (:type piece))
                           (util/king-castling-move? from to))]
      {:db (-> db
               (assoc-in [:board to] piece)
               (update :board dissoc from))
       :fx (cond->
               [[:dispatch [::change-turn]]
                [:dispatch [::log-move from to]]]
             promotion?  (conj [:dispatch [::promote to]])
             en-passant? (conj [:dispatch [::en-passant from to]])
             castling?   (conj [:dispatch [::castling to]]))})))

(rf/reg-event-fx
  ::move
  move-piece)

(defn select-square
  [{:keys [db]} [_ pos]]
  (let [selected                 (:selected db)
        piece-color              (fn [pos] (get-in db [:board pos :color]))
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
  ::promote
  (fn [db [_ pos]]
    (assoc-in db [:board pos :type] :queen)))

(rf/reg-event-db
  ::en-passant
  (fn [db [_ from to]]
    (update db :board dissoc (util/en-passant-pos from to))))

(rf/reg-event-db
  ::castling
  (fn [db [_ king-pos]]
    (let [[rook-from rook-to] (util/castling-rook-move king-pos)]
      (-> db
          (assoc-in [:board rook-to] (get-in db [:board rook-from]))
          (update :board dissoc rook-from)))))

(rf/reg-event-db
  ::log-move
  (fn [db [_ from to]]
    (update db :moves (fnil conj []) [from to])))
