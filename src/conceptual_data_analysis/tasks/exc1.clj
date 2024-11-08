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
             (if (str/blank? v)                             ; check if value is empty string
               [k nil]                                      ; replace empty string value with nil
               [k v]                                        ; value != empty string
               ))))


(defn get-deck-as-dict
  "Receives a file path and returns a hash-map of the cards in the file."
  [^String file-path]
  (let [cards (csv-files/csv-to-map file-path)              ; get cards as vector of hash-maps
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
    ;(println [new-card])
    (into {} [new-card])
    ;new-card
    ))


(defmulti compare-no-nil
          "Called if values are not nil.
          Dispatches on the key.
          Compares the values wrt. the compare-by value if key is not :game-name (lexicographic comparison)."
          (fn [key compare-by valA valB]
            key))                                           ;; Dispatch on the key itself

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
  :game-name                                                ; Lexicographic comparison
  [_ _ valA valB]
  ;(println "Not nil")
  ;(= 0 (compare valA valB))
  true
  )

(defn compare-nil
  " Compares two values. If one of the values is nil, the result is true. "
  [compare-by valA valB key]
  ;(println "NIL? " valA valB key)
  (if (some true? [(nil? valA) (nil? valB)])
    true                                                   ; if one of the values is nil, return true
    (compare-no-nil key compare-by valA valB)))             ; else compare the values

(defn compare-cards
  "Compares two cards by comparing each of their values wrt. the values of the keys defined in compare-by.
  Returns true if ALL the cards' values are in relation."
  [cardA cardB compare-by]
  ;(let [new-cardA (second (first (handle-date cardA)))                ; handle-date: year == last 4 characters
  ;      new-cardB (second (first  (handle-date cardB)))]               ; cardA/cardB are map whose sole value is a map -> vals: get inner map
  ;  (println "new card" new-cardA)
  ;(println (second  cardA))
  #_(println "RESULT" (:game-name (second cardA)) (:game-name (second cardB))
           (every? identity (map (fn [[key comp-fn]]               ; deconstruction: key & value extracted from element of compare-by
                            (compare-nil comp-fn (get (second cardA) key) (get (second cardB) key) key)) ; compare-nil: compare values of the cards
                          compare-by)))
    (every? identity (map (fn [[key comp-fn]]               ; deconstruction: key & value extracted from element of compare-by
                            (compare-nil comp-fn (get (second cardA) key) (get (second cardB) key) key)) ; compare-nil: compare values of the cards
                          compare-by)))
; )

(defn get-order-relation
  "Returns the order relation of the cards defined by their keys (game-name).
  cardA and cardB are compared by the compare-by value.
  They are in relation if cardA <= cardB.
  Only cards which are in relation are part of the vector returned."
  [compare-by deck]
  (into #{} (for [cardA deck
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
                    :max-num-players  <=
                    :min-age          <=
                    :inception        <=}
        order (get-order-relation compare-by deck)]
    ;(println cardA)
    ;(println cardB)
    ;(println compare-by)
    ;(println (compare-cards cardA cardB compare-by))
    (write-map-to-file order "resources/week1/order.edn")
    ;(write-map-to-file deck "resources/week1/deck.edn")
    (println "order" (count order))
    ;(println (read-map-from-file "resources/week1/order.edn"))
    ;(println (read-map-from-file "resources/week1/deck.edn"))
    )

;; task 3

; TODO: Implement the function

(defmulti
  convert-format-from-characteristic
  "Converts the format of the ordered set to the desired format."
  (fn [poset format]
    format))

(defmethod
  convert-format-from-characteristic
  :set-vectors
  [poset format]
  (get-order-relation (second poset) (first poset)))

(defmethod
  convert-format-from-characteristic
  :matrix                                                   ; implement as mapping
  [poset format]
  (into {} (for [cardA (first poset)]                                    ; key
    [(key cardA) (into {} (for [cardB (first poset)]
      [(key cardB) (compare-cards cardA cardB (second poset))]))]) )                ; compare cards
    )

(defmethod
  convert-format-from-characteristic
  :adjacency-list
  [poset format]
  (into {} (for [cardA (first poset)]                                    ; key
    [(key cardA) (into #{} (for [cardB (first poset)
                                 :when (compare-cards cardA cardB (second poset))]
      (key cardB)))])
        )                ; compare cards
        )

(defmulti convert-format-to-characteristic
          "Converts the format of the ordered set to the desired format."
  (fn [poset format]
    format))

(defmethod convert-format-to-characteristic
  :set-vectors
  [poset format]
  (println (second poset))
  #(.contains (second poset) [(first %1) (first %2)])
  )

(defmethod convert-format-to-characteristic
  :characteristic-function
  [poset format]
  poset)

(defmethod convert-format-to-characteristic                 ; TODO
  :default                                                  ; matrix & adjacency-list
  [poset format]
  (let [order-relation (second poset)
        data-type (type (first (vals order-relation)))]
    (println "order relation " order-relation)
    (if (= data-type clojure.lang.PersistentHashSet)
      #(.contains (order-relation (first %1)) (first %2))                                           ; adjacency-list
      #((order-relation (first %1)) (first %2))                                                    ; matrix
      )
    )
    )


(defn convert-format
  ""
  [ordered-set format]
  (convert-format-from-characteristic ordered-set format)
  )

(defn count-ones
  "Counts the number of ones in a nested map with values 0 or 1."
  [m]
  (reduce
    (fn [sum v]
      (cond
        (map? v) (+ sum (count-ones v)) ; If the value is a map, recurse into it
        (true? v ) (inc sum)               ; If the value is 1, increment the count
        :else sum))                     ; Otherwise, keep the count as-is
    0
    (vals m)))


(let [deck (read-map-from-file "resources/week1/deck.edn")
      order (read-map-from-file "resources/week1/order.edn")
      compare-by {:game-name        <=
                  :publication-date <=
                  :min-num-players  <=
                  :max-num-players  <=
                  :min-age          <=
                  :inception        <=}
      matrix-order (convert-format [deck compare-by] :matrix)
      adj-order (convert-format [deck compare-by] :adjacency-list)]
  ;(println (convert-format [deck compare-by] :set-vectors))
  ;(println (count-ones (convert-format [deck compare-by] :matrix)))
  ;(println (convert-format [deck compare-by] :adjacency-list))
  ;(println (reduce #(+ %1 (count (second %2))) 0 (convert-format [deck compare-by] :adjacency-list)))
  ;(println (first deck) (second deck))
  ;(println (convert-format-to-characteristic [deck order] :set-vectors))
  ;(println ((convert-format-to-characteristic [deck order] :set-vectors) (second deck) (first deck)))

  ;(println (convert-format-to-characteristic [deck matrix-order] :matrix))
  ;(println ((convert-format-to-characteristic [deck matrix-order] :matrix) (first deck) (second deck)))

  (println (convert-format-to-characteristic [deck adj-order] :adjacency-list))
  (println ((convert-format-to-characteristic [deck adj-order] :adjacency-list) (first deck) (second deck)))
  )

;(println (handle-date [["Magic: The Gathering" {:game-name "Magic: The Gathering", :publication-date 05.08.1993, :min-num-players 2, :max-num-players 2, :min-age nil, :inception 05.08.1993}]]))
















;; task 4
(defn reflexive?
  "Receives a base set and a relation.
  Returns true if the relation is reflexive."
  [base-set relation]
  (let [n (count base-set)
        reflexive (every? (fn [i] (= 1 (get-in relation [i i])))
                          (range n))]
    reflexive))

(defn anti-transitive? [base-set matrix]
  ; FIXME: This is not correct
  (let [n (count base-set)]
    (every? (fn [i]
              (every? (fn [j]
                        (every? (fn [k]
                                  (if (and (= 1 (get-in matrix [i j]))
                                           (= 1 (get-in matrix [j k])))
                                    (not= 1 (get-in matrix [k i]))) ; Checking for anti-transitivity
                                  true)                     ; Proceed even if the condition doesn't apply
                                (range n))
                        ) (range n))
              ) (range n))))


#_(defn order-relation?
  "Receives a base set and a relation.
  Returns true if the relation is an order relation."
  [base-set relation]

  (let [; reflexivity: use matrix: O(n)
        ; TODO: cast relation to matrix
        reflexive (reflexive? base-set relation)

        ;; antisymmetry: use matrix: O(n^2) ?
        anti-transitive (anti-transitive? base-set relation)


        ;; transitivity

        ]
    (println "reflexive? " reflexive)
    (println "anti-transitive? " anti-transitive)
    )
  )







#_(def matrix [[1 0 1]
             [0 1 0]
             [0 0 1]])

(def base-set [1 2 3])

;(println "reflexive (true): " (reflexive? base-set matrix)) ;; => true



#_(def non-reflexive-matrix [[0 0 0]
                           [0 1 0]
                           [0 0 1]])

;(println "reflexive (false): " (reflexive? base-set non-reflexive-matrix)) ;; => false


(def matrix [[0 1 0]
             [0 0 1]
             [0 0 0]])

;(println "anti-symmetric (true): " (anti-transitive? base-set matrix)) ;; => true

(def non-anti-transitive-matrix [[0 1 1]
                                 [0 0 1]
                                 [1 0 0]])

;(println "anti-symmetric (false): " (anti-transitive? base-set non-anti-transitive-matrix)) ;; => false