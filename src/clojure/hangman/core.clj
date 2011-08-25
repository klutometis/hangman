(ns hangman.core
  (:import (com.factual.hangman
            HangmanGame$Status))
  (:use clojure.contrib.pprint))

(defmacro debug-map [& exprs]
  `(zipmap (reverse (list ~@(map (fn [expr] `'~expr) exprs)))
           (reverse (list ~@(map (fn [expr] expr) exprs)))))

(defmacro debug-list [& exprs]
  `(list ~@(map (fn [expr] `(list '~expr ~expr)) exprs)))

(defmacro debug [& exprs]
  `(pprint (debug-map ~@exprs)))

(defn- can-keep-guessing?
  ([game] (= (.gameStatus game) HangmanGame$Status/KEEP_GUESSING)))

(defn run
  ([game strategy]
     ;; Can also just wrap this in with-open, since
     ;; assertCanKeepGuessing (in guessLetter and guessWord) will
     ;; throw an exception.
     (while (can-keep-guessing? game)
       (println game)
       (let [guess (.nextGuess strategy game)]
         (.makeGuess guess game)))
     (println game)
     (.currentScore game)))
