(ns conceptual-data-analysis.tasks.exc1
  (:require [conceptual-data-analysis.io.csv-files :as csv-files]
            [clojure.string :as str]
            ))

;; task 1
(defn replace-empty-str-w-nil
  "Receives a hash-map 'card' and replaces all values which are empty strings with nil.
  Returns a vector of key-value pairs/ vectors."
  [card]
  (into {} (for [[k v] card]
    (if (str/blank? v)                                      ; check if value is empty string
      [k nil]                                               ; replace empty string value with nil
      [k v]                                                 ; value != empty string
      ))))


(defn get-deck-as-dict
  "Receives a file path and returns a hash-map of the cards in the file."
  [^String file-path]
  (let [cards (csv-files/csv-to-map file-path)                        ; get cards as vector of hash-maps
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

(defn handle-date
  "Receives a mapping which represents cards."
  [card]
  (if (some true? [(nil? (:inception card)) (nil? (:publication-date card))])
    card
    (let [new-card (-> card
                        (assoc :inception (Integer/parseInt (subs (:inception card) (- (count (:inception card)) 4))))
                        (assoc :publication-date
                                  (Integer/parseInt (subs (:publication-date card)(- (count (:publication-date card)) 4))))
                         )]
      new-card)))

(defmulti compare-no-nil
          "Called if values are not nil."
         (fn [key compare-by a b]
    key))  ;; Dispatch on the key itself

(defmethod compare-no-nil
  :default
  ;; Called if key is not :game-name.
  ;; Compares the values wrt. the compare-by value.
  ;; Since the values are strings, they are converted to integers before comparison.
  [_ compare-by valA valB]
    (compare-by (Integer/parseInt valA) (Integer/parseInt valB)))

  (defmethod compare-no-nil
    (keyword "game-name")                                     ; Lexicographic comparison
  ;; Called if key is :game-name.
  ;; Compares the values of the cards lexicographically.
  ;; returns <0 if valA < valB, 0 if valA = valB, >0 if valA > valB in terms of length disregarding the content.
    [_ compare-by valA valB]
    (= 0 (compare valA valB)))

  (defn compare-nil
    " Compares two values. If one of the values is nil, the result is true. "
  [compare-by valA valB key]
  (if (some true? [(nil? valA) (nil? valB)])
    true
    (compare-no-nil key compare-by valA valB)))

(defn compare-cards
  [cardA cardB compare-by]
  (let [new-cardA (vals (handle-date cardA))
        new-cardB (vals (handle-date cardB))]
    (every? identity (map (fn [[key comp-fn]]
           (compare-nil comp-fn (get new-cardA key) (get new-cardB key) key))
         compare-by))))

(defn get-order-relation
  "Returns the order relation of the keys in the compare-by hash-map."
  [compare-by deck]
  (vec (for [cardA deck
             cardB deck
             :when (compare-cards cardA cardB compare-by)]
         [(key cardA) (key cardB)])))



;(println (compare-cards {:a 1 :b 2} {:a 1 :b 2}))           ; true
;(println (compare-cards {:a 1 :b 2} {:a 1 :b 3}))           ; true
;(println (compare-cards {:a 1 :b 3} {:a 1 :b 2}))           ; false
;(println (compare-cards {:a 1 :b " hello " :c " 2016 "} {:a 1 :b " hell " :c " 2017 "})) ; false
#_(let [s (:inception {:a 1 :b " hello " :c " 2016 " :inception " 08.2017 " (keyword " publication date ") " 02.03.2019 "})]
(println (Integer/parseInt (subs s (- (count s) 4)))))

;(println (handle-date (into {} (vals {(keyword "Uno") {:a 1 :b "hello" :c "2016" :inception "05.08.1993" :publication-date "05.08.1993"}}))))

(let [deck (get-deck-as-dict "resources/week1/card_games.csv")
      cardA (first deck)
      cardB (second deck)
      compare-by {:game-name        <=
                  :publication-date <=
                  :min-num-players  <=
                  :max-num-players <=
                  :min-age          <=
                  :inception                   <=}]
  ;(println cardA)
  ;(println cardB)
  ;(println compare-by)
  ;(println (compare-cards cardA cardB compare-by))
  (println (get-order-relation compare-by deck))
  )