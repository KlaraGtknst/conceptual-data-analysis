(ns conceptual-data-analysis.core
  (:require [conceptual-data-analysis.greetings :refer :all])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (do
   (println (say_hello args))
   (println (say_bye args))
   ))

(-main "Klara")