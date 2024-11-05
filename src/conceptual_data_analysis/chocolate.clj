(ns conceptual-data-analysis.chocolate)


(def me {:age 23 :name "klara" :favorite_chocolate_bar "twix"})

(defn chocolate_decision
  [person]
  (assoc person :prefers_duplo (boolean (= (:favorite_chocolate_bar person) "duplo")))
  )

(println (chocolate_decision me))