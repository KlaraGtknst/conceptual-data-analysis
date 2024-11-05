(ns conceptual-data-analysis.io.csv-files
  (:require [clojure-csv.core :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]))


(defn read-csv-file
  [file-path]
  (with-open [reader (io/reader file-path)]
    (doall
      (csv/parse-csv reader :delimiter \; :strict true))))

(defn csv-to-map
  [file-path]
  (let [data (read-csv-file file-path)
        header (map (comp keyword #(str/replace % " " "-") #(str/replace % #"^[^a-zA-Z0-9]+" "")) (first data))
        rows (rest data)]

    (mapv (fn [row] (zipmap header row)) rows))
  )


;(println (read-csv-file "resources/week1/card_games.csv"))
;(println (:game-name (first (csv-to-map "resources/week1/card_games.csv"))))
;(println (csv-to-map "resources/week1/card_games.csv"))

