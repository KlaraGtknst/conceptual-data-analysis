(ns conceptual-data-analysis.week1.exc1
  (:require [conceptual-data-analysis.io.csv-files :refer :all]
            [clojure.string :as str]
            ))

(defn replace-empty-str-w-nil
  [card]                                                    ; card is a map
  (into {} (for [[k v] card]
    (if (str/blank? v)
      [k nil]
      [k v]
      ))))


(defn get-deck-as-dict
  [file-path]
  (let [cards (csv-to-map file-path)
        nil-cards (mapv replace-empty-str-w-nil cards)]
    (println nil-cards)
    (reduce #(assoc %1 (:game-name %2) %2) {} nil-cards))
  )

;; cards as dict
#_(let [file-path "resources/week1/card_games.csv"]
  (println (csv-to-map file-path)))

;; whole deck as dict
(println ((get-deck-as-dict "resources/week1/card_games.csv") "Best Act"))
(println (get-deck-as-dict "resources/week1/card_games.csv"))