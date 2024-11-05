(ns conceptual-data-analysis.io.csv-files
  (:require [clojure-csv.core :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]))


(defn read-csv-file
  "Reads a csv file and returns a vector of vectors.
  Each inner vector represents a row in the csv file.
  Hence, the first inner vector contains the header of the csv file.
  The csv file is expected to have a semicolon as delimiter.
  If the delimiter is different, it can be passed as an argument."
  ([^String file-path delimiter]
  (with-open [reader (io/reader file-path)]
    (doall
      (csv/parse-csv reader :delimiter delimiter :strict true))))
  ([^String file-path]
  (read-csv-file file-path \;)))


(defn csv-to-map
  "Reads a csv file and returns a vector of maps."
  [^String file-path]
  (let [data (read-csv-file file-path)
        ; replace spaces with "-" and omit special characters in header (bc first header element has leading special character)
        header (map (comp keyword #(str/replace % " " "-") #(str/replace % #"^[^a-zA-Z0-9]+" "")) (first data))
        rows (rest data)]
    (mapv (fn [row] (zipmap header row)) rows))             ; every row is input for zipmap
                                                            ; zipmap creates a map from keys and values
                                                            ; header = keys, row = values
                                                            ; fn is an anonymous function
                                                            ; mapv creates a vector of maps
  )


;(println (read-csv-file "resources/week1/card_games.csv"))
;(println (:game-name (first (csv-to-map "resources/week1/card_games.csv"))))
;(println (csv-to-map "resources/week1/card_games.csv"))

