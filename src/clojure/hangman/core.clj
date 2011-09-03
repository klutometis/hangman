(ns hangman.core
  ^{:doc "Utilities for playing a hangman game common to all
  strategies."}
  (:import (com.factual.hangman
            HangmanGame
            HangmanGame$Status))
  (:use [clojure.contrib.pprint :only (pprint)]))

(defmacro debug-map
  "Map quoted expressions to their values."
  [& exprs]
  `(zipmap (reverse (list ~@(map (fn [expr] `'~expr) exprs)))
           (reverse (list ~@(map (fn [expr] expr) exprs)))))

(defmacro debug-list
  "List-associate expressions with their values."
  [& exprs]
  `(list ~@(map (fn [expr] `(list '~expr ~expr)) exprs)))

(defmacro debug [& exprs]
  "Pretty-print expression-value associations."
  `(pprint (debug-map ~@exprs)))

(defn- can-keep-guessing?
  ([game] (= (.gameStatus game) HangmanGame$Status/KEEP_GUESSING)))

(defmacro time-and-value [expr]
  "Return both the value and execution time for an expression."
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     [(/ (double (- (. System (nanoTime)) start#)) 1000000.0)
      ret#]))

(defn run-with-out-str [game strategy verbose?]
  "Capture the output of a game in a string and return it with the
score in a {:output ... :score ...} map."
  ;; Can also just wrap this in with-open, since
  ;; assertCanKeepGuessing (in guessLetter and guessWord) will
  ;; throw an exception.
  (let [output
        (with-out-str
          (while (can-keep-guessing? game)
            (if verbose? (println game))
            (let [guess (.nextGuess strategy game)]
              (.makeGuess guess game)))
          (if verbose? (println game)))]
    {:output output
     :score (.currentScore game)}))

(defn run [game strategy verbose?]
  "Run a game, returning its score in a {:output nil :score ...} map."
  ;; Can also just wrap this in with-open, since
  ;; assertCanKeepGuessing (in guessLetter and guessWord) will
  ;; throw an exception.
  (while (can-keep-guessing? game)
    (if verbose? (println game))
    (let [guess (.nextGuess strategy game)]
      (.makeGuess guess game)))
  (if verbose? (println game))
  {:output nil
   :score (.currentScore game)})

(defn average-vals [data key]
  "Average over the values of a map."
  (let [vals (map key (vals data))
        n-vals (count vals)]
    (if (zero? n-vals)
      0.0
      (float (/ (reduce + vals) (count vals))))))

(defn run-words
  "Run games over words with a given strategy; returning scores, times
and summary statistics."
  ([make-strategy
    arity->dictionary
    arity->letter->count
    additional-arguments
    max-wrong-guesses
    words]
     (run-words make-strategy
                arity->dictionary
                arity->letter->count
                additional-arguments
                max-wrong-guesses
                words
                false))
  ([make-strategy
    arity->dictionary
    arity->letter->count
    additional-arguments
    max-wrong-guesses
    words
    verbose?]
     (let [run
           (if verbose? run-with-out-str run)
           time-values
           (map (fn [word]
                  (let [arity (count word)
                        [time {:keys [output score]}]
                        (time-and-value
                         (run (new HangmanGame word max-wrong-guesses)
                              (make-strategy
                               (arity->dictionary arity)
                               (arity->letter->count arity)
                               (additional-arguments arity))
                              verbose?))]
                    (if output (print output))
                    {:time-in-ms time
                     :score score}))
                words)
           data (zipmap words time-values)
           average-time (average-vals data :time-in-ms)
           average-score (average-vals data :score)]
       {:data (into (sorted-map) data)
        :average-time-in-ms average-time
        :average-score average-score})))
