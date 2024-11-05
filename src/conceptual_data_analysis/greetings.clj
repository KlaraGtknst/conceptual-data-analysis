(ns conceptual-data-analysis.greetings)

(defn say_hello
  "prints hello"
  ([^String name]
   (str "Hello " name))
  ([]
  (str "Hello Klara")))

(defn say_bye
  "prints bye"
  ([^String name]
   (str "Bye " name))
  ([]
  (str "Bye Klara")))

;(println (say_hello))
;(println (say_hello "Marie"))
;(println (say_bye))
;(println (say_bye "Johannes"))
