(ns conceptual-data-analysis.tasks.exc1
  (:require [conceptual-data-analysis.io.csv-files :refer :all]
            [clojure.string :as str]
            ))

;; task 1
(defn replace-empty-str-w-nil
  "Receives a hash-map 'card' and replaces all values which are empty strings with nil.
  Returns a vector of key-value pairs/ vectors."
  [^hash-map card]
  (into {} (for [[k v] card]
    (if (str/blank? v)                                      ; check if value is empty string
      [k nil]                                               ; replace empty string value with nil
      [k v]                                                 ; value != empty string
      ))))


(defn get-deck-as-dict
  "Receives a file path and returns a hash-map of the cards in the file."
  [^String file-path]
  (let [cards (csv-to-map file-path)                        ; get cards as vector of hash-maps
        nil-cards (mapv replace-empty-str-w-nil cards)]     ; replace empty strings values with nil
    (reduce #(assoc %1 (:game-name %2) %2) {} nil-cards))   ; reduce: first input is empty hash-map,
                                                            ;         second input (nil-cards) is a vector of hash-maps
                                                            ;         first result is combined with second result
                                                            ; assoc: add key-value pair to hash-map
                                                            ; game-name is the key in the hash-map
                                                            ; %1 is resulting hash-map, %2 is a hash-map from the vector
  )

;; example
;; cards as dict
#_(let [file-path "resources/week1/card_games.csv"]
  (println (csv-to-map file-path)))

;; whole deck as dict
;(println ((get-deck-as-dict "resources/week1/card_games.csv") "Best Act"))
;(println (get-deck-as-dict "resources/week1/card_games.csv"))


;; task 2
(defn compare-cards
  [cardA cardB]
  (every? identity (map #(<= %1 %2) (vals cardA) (vals cardB)))) ; TODO: csv values from str to int & handel date

(println (compare-cards {:a 1 :b 2} {:a 1 :b 2}))           ; true
(println (compare-cards {:a 1 :b 2} {:a 1 :b 3}))           ; true
(println (compare-cards {:a 1 :b 3} {:a 1 :b 2}))           ; false