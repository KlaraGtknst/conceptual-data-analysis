(ns conceptual-data-analysis.tasks.exc1
  (:require [clojure.set :as set]
            [conceptual-data-analysis.io.csv-files :as csv-files]
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
  ;(= 0 (compare valA valB))
  true
  )

(defn compare-nil
  " Compares two values. If one of the values is nil, the result is true. "
  [compare-by valA valB key]
  ;(println valA valB)
  (if (some true? [(nil? valA) (nil? valB)])
    true                                                   ; if one of the values is nil, return true
    (compare-no-nil key compare-by valA valB)))             ; else compare the values

(defn compare-cards
  "Compares two cards by comparing each of their values wrt. the values of the keys defined in compare-by.
  Returns true if ALL the cards' values are in relation."
  [cardA cardB compare-by]
  ;(let [new-cardA (second (first (handle-date cardA)))                ; handle-date: year == last 4 characters
  ;      new-cardB (second (first  (handle-date cardB)))]               ; cardA/cardB are map whose sole value is a map -> vals: get inner map

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
    ;(write-map-to-file order "resources/week1/order.edn")
    ;(write-map-to-file deck "resources/week1/deck.edn")
    ;(println "order" order)
    ;(println (read-map-from-file "resources/week1/order.edn"))
    ;(println (read-map-from-file "resources/week1/deck.edn"))
    )

;; task 3
(defmulti
  convert-format-from-characteristic
  "Converts the format of the ordered set to the desired format."
  (fn [poset format]                                        ; poset: Base-set, order-relation
    format))

(defmethod
  convert-format-from-characteristic                        ; O(N^2), bc of for-loop two times over the base-set
  :set-vectors
  [poset format]
  [(first poset) (into #{} (for [cardA (vals (first poset))
                     cardB (vals (first poset))
                     :when ((second poset) cardA cardB)]  ; :when: filter -> only return card <= cardB pairs
                 [(:game-name cardA) (:game-name cardB)]))])

(defmethod
  convert-format-from-characteristic                        ;O(N^2), bc of for-loop two times over the base-set
  :matrix                                                   ; implement as mapping
  [poset format]
  ;(println "basset" (second poset))
  [(first poset)
   (into {} (for [cardA (vals (first poset))]                                    ; key
    [(:game-name cardA)
     (into {} (for [cardB (vals (first poset))]
      [(:game-name cardB)
       ((second poset) cardA cardB)]))]))
   ]               ; compare cards
    )

(defmethod
  convert-format-from-characteristic                        ; O(N^2), bc of for-loop two times over the base-set
  :adjacency-list
  [poset format]
  ;(println "poset" ((second poset) (first (vals (first poset))) (first (vals (first poset)))))
  ;(println (convert-format-from-characteristic poset :set-vectors))
  ;(println "Element: " (first (vals (first poset))))
  ;(println "Vektor" (for [cardB (vals (first poset))
  ;                               :when ((second poset) (first (vals (first poset))) (first (vals (first poset))) )]
  ;    (:game-name cardB)))
  ;(println "Test123" (into #{} (for [cardB (vals (first poset))
  ;                               :when ((second poset) (first (vals (first poset)))  (second (vals (first poset))) )]
  ;    (:game-name cardB))))
  [(first poset)
   (into {} (for [cardA (vals (first poset))]                                    ; key
    [(:game-name cardA) (into #{} (for [cardB (vals (first poset))
                                 :when ((second poset) cardA cardB )]
      (:game-name cardB)))])
        )
   ]
        )

(defmethod
  convert-format-from-characteristic                        ; O(1), returns the characteristic function
  :characteristic-function
  [poset format]                                            ; poset: Base-set, order-relation
  poset)



(defn convert-format-to-characteristic
  " A characteristic functions receives mappings of two cards, i.e. only inner map, and returns true if in relation."
  [poset]
  (let [order-relation (second poset)]
    (if (= (type order-relation) clojure.lang.PersistentHashSet)
      [(first poset) #(.contains (second poset) [(:game-name %1) (:game-name %2)])] ; set of vectors -> O(N^2) lookup via contains (run over all elements)

       (if (or (= (type order-relation) clojure.lang.PersistentHashMap) (= (type order-relation) clojure.lang.PersistentArrayMap))
         (if (= (type (first (vals order-relation))) clojure.lang.PersistentHashSet)
           [(first poset) #(.contains (get order-relation (:game-name %1)) (:game-name %2))]        ; adjacency-list -> O(log(N) + N) get hashset in O(log(N)) cf. Clojure lecture, lookup via contains (run over all <=N elements)
           [(first poset) #(get (get order-relation (:game-name %1)) (:game-name %2))]              ; matrix -> O(log(N) + log(N)) get hashset in O(log(N)) cf. Clojure lecture
          )
         poset                                              ; characteristic-function -> return the characteristic function: O(1)
         )
    )
    )
  )



(defn convert-format                                        ; O(N^2)
  "Takes a poset and a format and converts the poset to the desired format.
  Returns the converted poset.
  Poset has the format [base-set order-relation].
  First, the characteristic function is determined.
  Then, the format is converted to the desired format."
  [poset format]
  ;(println "base-set" (first poset))
  (let [poset-characteristic (convert-format-to-characteristic poset)] ; O(N^2)
    ;(println "res order" ((second poset-characteristic) (first (vals (first poset))) (second (vals (first poset)))))
  (convert-format-from-characteristic poset-characteristic format) ; O(N^2)
  ))

(defn count-ones
  "Counts the number of ones in a nested map with values 0 or 1.
  Helper function for testing."
  [m]
  (reduce
    (fn [sum v]
      (cond
        (map? v) (+ sum (count-ones v)) ; If the value is a map, recurse into it
        (true? v ) (inc sum)               ; If the value is 1, increment the count
        :else sum))                     ; Otherwise, keep the count as-is
    0
    (vals m)))

;; test task 3
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

  ;(println "matr" (second matrix-poset))
    ;(println "adj" (second adj-poset))
  ;; matr -> charac
  ;(println "111" (second (convert-format matrix-poset :characteristic-function)))
  ;(println "RESULT" (first (vals deck)) (second (vals deck)))
  ;(println ((second (convert-format matrix-poset :characteristic-function)) (second (vals deck)) (first (vals deck))))

  ;; adj -> charac
  ;(println (convert-format adj-poset :characteristic-function))
  ;(println ((second (convert-format adj-poset :characteristic-function)) (first (vals deck)) (second (vals deck))))

  ;; set-vectors -> charac
  ;(println (= clojure.lang.PersistentHashSet (type (second set-vectors-poset))))
  ;(println (second (convert-format set-vectors-poset :characteristic-function)))
  ;(println "set-vec -> char" ((second (convert-format set-vectors-poset :characteristic-function)) (first (vals deck)) (second (vals deck))))

  ;; charac -> charc
  ;(println (convert-format characteristic-poset :characteristic-function))
  ;(println "Char" ((second (convert-format characteristic-poset :characteristic-function)) (first (vals deck)) (second (vals deck))))


  ;; matr -> matr
    ;(println "matr" (second (convert-format matrix-poset :matrix)))
  ;(println "Matr " (((second (convert-format matrix-poset :matrix)) (first (keys deck))) (second (keys deck))))

  ;; set-vectors -> set-vectors
  ;(println (second set-vectors-poset))
    ;(println (second (convert-format set-vectors-poset :set-vectors)))
    ;(println "set-vec " (.contains (second (convert-format set-vectors-poset :set-vectors)) [(first (keys deck)) (second (keys deck))]))

  ;; adj -> adj
  ;(println "adj" (second (convert-format adj-poset :adjacency-list)))
  ;(println "Adj " (.contains (get (second (convert-format adj-poset :adjacency-list)) (:game-name (first (vals deck)))) (:game-name (second (vals deck)))))
  )




;; task 4
(defn reflexive?                                            ; O(N*log(N)), bc of for-loop over the base-set and get in O(log(N)) cf. Clojure lecture
  "Receives a base set and a relation.
  Returns true if the relation is reflexive."
  [base-set relation]
  (every? identity (map (fn [card] (get (get relation (:game-name card)) (:game-name card)))
                          (vals base-set))))


(defn anti-symmetric? [base-set matrix]                     ; O(N^2*log(N)*2), bc of for-loop two times over the base-set
  (every? identity (for [cardA (vals base-set)
                         cardB (vals base-set)
                         :when (and (get (get matrix (:game-name cardA)) (:game-name cardB))
                                    (get (get matrix (:game-name cardB)) (:game-name cardA)))]
                     (= (:game-name cardA) (:game-name cardB))))
  )

(defn transitive?                                           ; O(N^3log(N)*3), bc of for-loop three times over the base-set
  "Receives a base set and a relation.
  Returns true if the relation is transitive."
  [base-set relation]
  (every? identity (for [cardA (vals base-set)
                         cardB (vals base-set)
                         cardC (vals base-set)
                         :when (and (get (get relation (:game-name cardA)) (:game-name cardB))
                                    (get (get relation (:game-name cardB)) (:game-name cardC)))]
                     (get (get relation (:game-name cardA)) (:game-name cardC))))
  )


(defn order-relation?                                       ; O(N^3log(N)*3), bc of transitive? is O(N^3log(N)*3)
  "Receives a base set and a relation.
  Returns true if the relation is an order relation."
  [base-set relation]
  (let [matr-order (second (convert-format [base-set relation] :matrix)) ; convert to matrix

        ;; reflexivity
        reflexive (reflexive? base-set matr-order)

        ;; antisymmetry: use matrix: O(n^2) ?
        anti-symmetric (anti-symmetric? base-set matr-order)


        ;; transitivity
        transitive (transitive? base-set matr-order)
        ]
    (println "reflexive? " reflexive)
    (println "anti-symmetric? " anti-symmetric)
    (println "transitive? " transitive)
    (and reflexive anti-symmetric transitive)
    )
  )



;; test task 4

(def order-relation-matrix {"Uno" {"Uno" true "skat" false "Poker" false}
             "skat" {"Uno" false "skat" true "Poker" false}
             "Poker" {"Uno" false "skat" false "Poker" true}})

(def not-ref-matrix {"Uno" {"Uno" true "skat" false "Poker" false}
             "skat" {"Uno" false "skat" true "Poker" false}
             "Poker" {"Uno" false "skat" false "Poker" false}})

(def not-antisym-matrix {"Uno" {"Uno" true "skat" false "Poker" true}
             "skat" {"Uno" false "skat" true "Poker" false}
             "Poker" {"Uno" true "skat" false "Poker" true}})

(def not-trans-matrix {"Uno" {"Uno" true "skat" true "Poker" false}
             "skat" {"Uno" false "skat" true "Poker" true}
             "Poker" {"Uno" false "skat" false "Poker" true}})

(def base-set {"Uno" {:game-name "Uno" :a 12 :b 10}
               "skat" {:game-name "skat" :a 12 :b 10}
               "Poker" {:game-name "Poker" :a 12 :b 10}})

;(println "order-relation (true): " (order-relation? base-set order-relation-matrix)) ;; => true
;(println "not-ref-relation (false): " (order-relation? base-set not-ref-matrix)) ;; => false
;(println "not-antisym-relation (false): " (order-relation? base-set not-antisym-matrix)) ;; => false
;(println "not-trans-relation (false): " (order-relation? base-set not-trans-matrix)) ;; => false


;; task 5: Pareto Optima == maximum of order relation (e.g. no nachfolger), bc an element is better than another if it is better in all aspects

(defn get-pareto-optima
  "Receives a base set and an order relation.
  Returns the Pareto optima of the order relation.
  Returns the best (if smallest is best) cards in the order relation wrt. all attributes together.
  There might be better cards in one attribute, but not in all attributes.
  Hence, a strict optimal card wrt. one attribute order relation (!= all attributes) is a Pareto optimum."
  [poset]
  ;(println "base-set222" (first poset))
  ;(println "INIT" (second (convert-format poset :adjacency-list)))
  (let [base-set (first poset)
        relation (second poset)
        adj-order (second (convert-format [base-set relation] :adjacency-list)) ; convert to adjacency-list
        pareto-optima (into #{} (for [cardA (vals base-set)
                                      :when (= 1 (count (get adj-order (:game-name cardA))))]
                        (:game-name cardA)))]
    ;(println "adj-order from relation" adj-order)
    pareto-optima
    )
  )


(def deck (read-map-from-file "resources/week1/deck.edn"))
(def order (read-map-from-file "resources/week1/order.edn"))
(def compare-by {:game-name        <=
                  :publication-date <=
                  :min-num-players  <=
                  :max-num-players  <=
                  :min-age          <=
                  :inception        <=})
(def matrix-poset (convert-format [deck order] :matrix))
(def adj-poset (convert-format [deck order] :adjacency-list))
(def characteristic-poset [deck #(compare-cards %1 %2 compare-by)])
(def set-vectors-poset [deck order])
;(println "adj-relation" (second adj-poset))
;(println "Pareto Optima: " (get-pareto-optima adj-poset))



;; task 6: dual order relation

(defn get-dual-poset
  "Receives a base set and an order relation.
  Returns the dual order relation of the order relation.
  The dual order relation is the order relation with reversed order."
  [base-set relation]
  (let [[base-set characteristic-relation] (convert-format [base-set relation] :characteristic-function)]
    [base-set #(characteristic-relation %2 %1)]))

;(println ((second (get-dual-poset base-set not-trans-matrix)) {:game-name "Uno"} {:game-name "skat"})) ;; false
;(println ((second (get-dual-poset base-set not-trans-matrix)) {:game-name "skat"} {:game-name "Uno"})) ;; true
;(println "Pareto Optima: " (get-pareto-optima (get-dual-poset (first adj-poset) (second adj-poset)))) ; fair, bc no "worst" card

;; task 7: Order-tree
(defn tree?
  "Receives a base set and an order relation.
  Returns true if the order relation is a tree.
  A tree has only one predecessor for each card and one connected component.
  One connected component: Only one pareto optima of dual order.
  One predecessor: Each card has only one card as predecessor."
  [base-set relation]
  (let [[base-set dual-order] (get-dual-poset base-set relation) ; get dual order-relation
        is-no-forest (= 1 (count(get-pareto-optima [base-set dual-order]))) ; only one connected component
        ;; reflexive and one predecessor -> 2 predecessor in adjacency-list
        one-pred (every? identity (map #(= 2 (count ((second (convert-format [base-set dual-order] :adjacency-list)) %))) (keys base-set)))]
   (and is-no-forest one-pred)
  )
)

;(println "Tree? " (tree? deck order)) ; original order-relation
;(println "Tree? " (tree? deck (second (get-dual-poset deck order)))) ; dual order-relation
;(println (count (get-pareto-optima [{} order])))


;; task 8: linear extension
(defn get-linear-extension
  [base-set relation]
  (let [dual-order (second (convert-format (get-dual-poset base-set relation) :adjacency-list))
        order (second (convert-format [base-set relation] :adjacency-list))]

    (loop [le-order []
           dual-order dual-order
           non-zero-base-set base-set]
      (if (empty? non-zero-base-set)
        [base-set (into #{} (map #(into [] (vec %)) (partition 2 1 le-order)))] ; set of vectors
        (let [min-indegree (apply min (map #(count (get dual-order %)) (keys non-zero-base-set)))
              zero-indegree-nodes (filter #(= min-indegree (count (get dual-order %))) (keys non-zero-base-set))
              new-base-set (apply dissoc non-zero-base-set zero-indegree-nodes)
              ]
          (recur (concat le-order (shuffle zero-indegree-nodes))
                 (apply dissoc dual-order zero-indegree-nodes)
                 new-base-set))))
      )
    )

(defn get-intersection
  [posets]
  (let [base-set (first (first posets))
        adj-relations (map #(second (convert-format % :set-vectors)) posets)
        intersection (reduce (fn [acc x] (set/intersection acc x)) (first adj-relations) (rest adj-relations))
        ]
    [base-set intersection])
  )

;; task 9
(defn get-difference
  [poset1 poset2]
  (let [base-set (first poset1)
        set-vec-relation1 (second (convert-format poset1 :set-vectors) )
        set-vec-relation2 (second (convert-format poset2 :set-vectors) )
        difference (set/difference set-vec-relation1 set-vec-relation2)
        ]
    [base-set difference])
  )

;; task 9 : experiment
(defn experiment
  [poset]
  (let [[base-set order-relation] poset
   ]

    (loop [linear-extensions [(get-linear-extension base-set order-relation)]
           differences []
           ]
      (let [realizer (get-intersection linear-extensions) ; vector of posets
            dif (second (get-difference realizer poset))]
        ;(println "difference" (count dif) (count linear-extensions) dif)
        (if (empty? dif)
          (do
            (spit "resources/week1/linear-extensions.txt" (str linear-extensions "\n") :append false)
            (spit "resources/week1/differences.txt" (str differences "\n") :append true)
            (count linear-extensions))
          (recur (conj linear-extensions (get-linear-extension base-set order-relation))
                 (conj differences (count dif))
                 )
          )

        )

      )))


(println "\n ------------------Task 8-----------------")
;(println (second (convert-format (get-dual-poset (first adj-poset) (second adj-poset)) :adjacency-list)))
(let [dual (second (convert-format (get-dual-poset (first adj-poset) (second adj-poset)) :adjacency-list))
      lin-ex1  (get-linear-extension (first adj-poset) (second adj-poset) )
      lin-ex2  (get-linear-extension (first adj-poset) (second adj-poset) )
      lin-ex3 (get-linear-extension (first adj-poset) (second adj-poset) )
      intersection-lin-ex (second (get-intersection [lin-ex1 lin-ex2 lin-ex3]))
      diff-lin-1-2 (get-difference lin-ex1 lin-ex2)]
  ;(println "BASE" (first lin-ex1))
  ;(println "result 1" (second lin-ex1))
  ;(println (count (second lin-ex1)))
  ;(println "result 2" (second lin-ex2))
  ;(println (count (second lin-ex2)))
  ;(println "result 3" (second lin-ex3))
  ;(println (count (second lin-ex3)))
  ;(println "intersection-lin-ex" intersection-lin-ex)
  ;(println (count intersection-lin-ex)
  ;
  ;(println "\n ------------------Task 9-----------------")
  ;(println "difference1" diff-lin-1-2)
  ;(println "difference" (second diff-lin-1-2) )
  ;(println "num elements in difference" (count (second diff-lin-1-2)))

  (println "\n ------------------Task 9: Experiment-----------------")
  (let [difference-counts (vec (repeatedly 10 #(experiment adj-poset)))]
    (println difference-counts)
    (println "Experiment" (/ (float (apply + difference-counts)) (count difference-counts))))

  )


