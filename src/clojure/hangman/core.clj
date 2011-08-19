(ns hangman.core
  (:import (com.factual.hangman
            HangmanGame$Status)))

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
