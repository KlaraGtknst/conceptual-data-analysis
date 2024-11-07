(ns conceptual-data-analysis.tasks.exc1
  (:require [conceptual-data-analysis.io.csv-files :as csv-files]
            [clojure.string :as str]
            [clojure.edn :as edn]
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
  "Receives a (shallow) mapping which represents a card. If :inception or :publication-date are present, converts
  the last 4 characters to an integer year."
  [card]
  (let [new-card (-> card
                     (cond-> (:inception card)
                       (assoc :inception (Integer/parseInt (subs (:inception card) (- (count (:inception card)) 4)))))
                     (cond-> (:publication-date card)
                       (assoc :publication-date (Integer/parseInt (subs (:publication-date card) (- (count (:publication-date card)) 4))))))]
    new-card))


(defmulti compare-no-nil
    "Called if values are not nil.
    Dispatches on the key.
    Compares the values wrt. the compare-by value if key is not :game-name (lexicographic comparison)."
   (fn [key compare-by valA valB]
    key))  ;; Dispatch on the key itself

  ;; Called if key is not :game-name.
  ;; Compares the values wrt. the compare-by value.
  ;; Since the values are strings, they are converted to integers before comparison.
(defmethod compare-no-nil
  :default                                                  ; everything that can be converted to integer
  [_ compare-by valA valB]
    (compare-by (Integer/parseInt valA) (Integer/parseInt valB))) ; compare-by is a function chosen by the user

  ;; Called if key is :game-name.
  ;; Compares the values of the cards lexicographically.
  ;; returns <0 if valA < valB, 0 if valA = valB, >0 if valA > valB in terms of length disregarding the content:
  ;; https://clojuredocs.org/clojure.core/compare (07.11.2024)
  (defmethod compare-no-nil
    :game-name                                     ; Lexicographic comparison
    [_ _ valA valB]
    (= 0 (compare valA valB)))

  (defn compare-nil
    " Compares two values. If one of the values is nil, the result is true. "
  [compare-by valA valB key]
  (if (some true? [(nil? valA) (nil? valB)])
    true                                                    ; if one of the values is nil, return true
    (compare-no-nil key compare-by valA valB)))             ; else compare the values

(defn compare-cards
  "Compares two cards by comparing each of their values wrt. the values of the keys defined in compare-by.
  Returns true if ALL the cards' values are in relation."
  [cardA cardB compare-by]
  (let [new-cardA (vals (handle-date cardA))                ; handle-date: year == last 4 characters
        new-cardB (vals (handle-date cardB))]               ; cardA/cardB are map whose sole value is a map -> vals: get inner map
    (every? identity (map (fn [[key comp-fn]]               ; deconstruction: key & value extracted from element of compare-by
           (compare-nil comp-fn (get new-cardA key) (get new-cardB key) key)) ; compare-nil: compare values of the cards
         compare-by))))

(defn get-order-relation
  "Returns the order relation of the cards defined by their keys (game-name).
  cardA and cardB are compared by the compare-by value.
  They are in relation if cardA <= cardB.
  Only cards which are in relation are part of the vector returned."
  [compare-by deck]
  (vec (for [cardA deck
             cardB deck
             :when (compare-cards cardA cardB compare-by)]  ; :when: filter -> only return card <= cardB pairs
         [(key cardA) (key cardB)])))                       ; save only game-names



(defn write-map-to-file
  "Writes a map/ vectors to a file in EDN format."
  [m filename]
  ; spit: Opposite of slurp. Opens f with writer, writes content, then closes f.
  (spit filename (pr-str m)))                               ; pr-str: returns a string representation of the object

(defn read-map-from-file
  "Reads a map/ vectors from a file in EDN format."
  [filename]
  ; slurp: Opens a reader on f and reads all its content into a string
  (edn/read-string (slurp filename)))                       ; read-string: reads the string as EDN




;; examples
;(println (compare-cards {:a 1 :b 2} {:a 1 :b 2}))           ; true
;(println (compare-cards {:a 1 :b 2} {:a 1 :b 3}))           ; true
;(println (compare-cards {:a 1 :b 3} {:a 1 :b 2}))           ; false
;(println (compare-cards {:a 1 :b " hello " :c " 2016 "} {:a 1 :b " hell " :c " 2017 "})) ; false
#_(let [s (:inception {:a 1 :b " hello " :c " 2016 " :inception " 08.2017 " (keyword " publication date ") " 02.03.2019 "})]
(println (Integer/parseInt (subs s (- (count s) 4)))))

;; test handle-date
;(println (handle-date (into {} (vals {(keyword "Uno") {:a 1 :b "hello" :c "2016" :inception "05.08.1993" :publication-date "05.08.1993"}}))))
;(println (handle-date (into {} (vals {(keyword "Uno") {:a 1 :b "hello" :c "2016" :inception nil :publication-date "05.08.1993"}}))))
;(println (handle-date (into {} (vals {(keyword "Uno") {:a 1 :b "hello" :c "2016" :inception "05.08.1993" :publication-date nil}}))))
;(println (handle-date (into {} (vals {(keyword "Uno") {:a 1 :b "hello" :c "2016" :inception nil :publication-date nil}}))))

(let [deck (get-deck-as-dict "resources/week1/card_games.csv")
      cardA (first deck)
      cardB (second deck)
      compare-by {:game-name        <=
                  :publication-date <=
                  :min-num-players  <=
                  :max-num-players <=
                  :min-age          <=
                  :inception                   <=}
      order (get-order-relation compare-by deck)]
  ;(println cardA)
  ;(println cardB)
  ;(println compare-by)
  ;(println (compare-cards cardA cardB compare-by))
  ;(write-map-to-file order "resources/week1/order.edn")
  ;(write-map-to-file deck "resources/week1/deck.edn")
  ;(println order)
  ;(println (read-map-from-file "resources/week1/order.edn"))
  ;(println (read-map-from-file "resources/week1/deck.edn"))
  )