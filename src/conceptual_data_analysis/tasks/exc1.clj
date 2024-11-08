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

  (println compare-by)
    (every? identity (map (fn [[attribute comp-fn]]               ; deconstruction: key (attribute) & value extracted from element of compare-by
                            (compare-nil comp-fn (attribute cardA) (attribute cardB) attribute)) ; compare-nil: compare values of the cards
                          compare-by)))
; )

(defn get-order-relation
  "Returns the order relation of the cards defined by their keys (game-name).
  cardA and cardB are compared by the compare-by value.
  They are in relation if cardA <= cardB.
  Only cards which are in relation are part of the vector returned."
  [compare-by deck]
  (into #{} (for [cardA (vals deck)
             cardB (vals deck)
             :when (compare-cards cardA cardB compare-by)]  ; :when: filter -> only return card <= cardB pairs
         [(:game-name cardA) (:game-name cardB)])))                       ; save only game-names



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
    (println "order" order)
    ;(println (read-map-from-file "resources/week1/order.edn"))
    ;(println (read-map-from-file "resources/week1/deck.edn"))
    )

;; task 3

; TODO: Implement the function

(defmulti
  convert-format-from-characteristic
  "Converts the format of the ordered set to the desired format."
  (fn [poset format]                                        ; poset: Base-set, order-relation
    format))

(defmethod
  convert-format-from-characteristic
  :set-vectors
  [poset format]
  [(first poset) (into #{} (for [cardA (vals (first poset))
                     cardB (vals (first poset))
                     :when ((second poset) (:game-name cardA) (:game-name cardB))]  ; :when: filter -> only return card <= cardB pairs
                 [(:game-name cardA) (:game-name cardB)]))])

(defmethod
  convert-format-from-characteristic
  :matrix                                                   ; implement as mapping
  [poset format]
  [(first poset)
   (into {} (for [cardA (vals (first poset))]                                    ; key
    [(:game-name cardA)
     (into {} (for [cardB (vals (first poset))]
      [(:game-name cardB)
       ((second poset) cardA cardB)]))]))
   ]               ; compare cards
    )

(defmethod
  convert-format-from-characteristic
  :adjacency-list
  [poset format]
  [(first poset)
   (into {} (for [cardA (vals (first poset))]                                    ; key
    [(:game-name cardA) (into #{} (for [cardB (vals (first poset))
                                 :when ((second poset) (:game-name cardA) (:game-name cardB) )]
      (:game-name cardB)))])
        )
   ]
        )

(defmethod
  convert-format-from-characteristic
  :characteristic-function
  [poset format]                                            ; poset: Base-set, order-relation
  poset)



(defn convert-format-to-characteristic
  " A characteristic functions receives mappings of two cards, i.e. only inner map, and returns true if in relation."
  [poset]

  (let [order-relation (second poset)]
    (println "order-relation" order-relation)
    (println "poset" (first (vals (first poset))) (second (vals (first poset))))
    (println (#(.contains (second poset) [(:game-name %1) (:game-name %2)]) (second (vals (first poset))) (first (vals (first poset)))))


    (if (= (type order-relation) clojure.lang.PersistentHashSet)
      [(first poset) #(.contains (second poset) [(:game-name %1) (:game-name %2)])] ; set of vectors

       (if (= (type order-relation) clojure.lang.PersistentHashMap)
         (if (= (type (first (vals order-relation))) clojure.lang.PersistentHashSet)
            [(first poset) #(.contains ((:game-name %1) order-relation) (:game-name %2))   ]                                        ; adjacency-list
            [(first poset) #((:game-name %2) ((:game-name %1) order-relation))   ]                                                 ; matrix
          )
         poset                                              ; characteristic-function
         )
    )
    )
  )



(defn convert-format
  ""
  [poset format]
  (let [poset-characteristic (convert-format-to-characteristic poset)]
  (convert-format-from-characteristic poset-characteristic format)

  ))

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
      matrix-poset (convert-format [deck order] :matrix)
      adj-poset (convert-format [deck order] :adjacency-list)
      characteristic-poset [deck #(compare-cards %1 %2 compare-by)]
      set-vectors-poset [deck order]]

    (println "matr" (second matrix-poset))
    (println "adj" (second adj-poset))
  ;; matr -> charac
  ;(println "111" (second (convert-format matrix-poset :characteristic-function)))
  ;(println "RESULT" (first (vals deck)) (second (vals deck)))
  ;(println ((second (convert-format matrix-poset :characteristic-function)) (second (vals deck)) (first (vals deck))))

  ;; adj -> charac
  ;(println (convert-format adj-poset :characteristic-function))
  ;(println ((second (convert-format adj-poset :characteristic-function)) (first (vals deck)) (second (vals deck))))

  ;; set-vectors -> charac
  (println (= clojure.lang.PersistentHashSet (type (second set-vectors-poset))))
  (println (second (convert-format set-vectors-poset :characteristic-function)))
  (println "set-vec -> char" ((second (convert-format set-vectors-poset :characteristic-function)) (first (vals deck)) (second (vals deck))))

  ;; charac -> charc
  ;(println (convert-format characteristic-poset :characteristic-function))
  ;(println "Char" ((second (convert-format characteristic-poset :characteristic-function)) (first (vals deck)) (second (vals deck))))


  ;; matr -> matr
  ;(println (second (convert-format matrix-poset :matrix)))
  ;(println "Matr " (((second (convert-format matrix-poset :matrix)) (first (keys deck))) (second (keys deck))))

  ;; set-vectors -> set-vectors
  (println (second set-vectors-poset))
  (println (second (convert-format set-vectors-poset :set-vectors)))
  (println "set-vec " (.contains (second (convert-format set-vectors-poset :set-vectors)) [(first (keys deck)) (second (keys deck))]))

  ;; adj -> adj
  (println (convert-format adj-poset :adjacency-list))
  (println "Adj " ((second (convert-format adj-poset :adjacency-list)) (first (vals deck)) (second (vals deck))))
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