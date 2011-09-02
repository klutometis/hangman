(ns hangman.core
  (:import (com.factual.hangman
            HangmanGame
            HangmanGame$Status))
  (:use clojure.contrib.pprint
        hangman.frequency-strategy
        hangman.regex-strategy))

(defmacro debug-map
  [& exprs]
  `(zipmap (reverse (list ~@(map (fn [expr] `'~expr) exprs)))
           (reverse (list ~@(map (fn [expr] expr) exprs)))))

(defmacro debug-list
  [& exprs]
  `(list ~@(map (fn [expr] `(list '~expr ~expr)) exprs)))

(defmacro debug [& exprs]
  `(pprint (debug-map ~@exprs)))

(defn- can-keep-guessing?
  ([game] (= (.gameStatus game) HangmanGame$Status/KEEP_GUESSING)))

(defmacro time-and-value [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     [(/ (double (- (. System (nanoTime)) start#)) 1000000.0)
      ret#]))

(defn run [game strategy verbose?]
  ;; Can also just wrap this in with-open, since
  ;; assertCanKeepGuessing (in guessLetter and guessWord) will
  ;; throw an exception.
  (while (can-keep-guessing? game)
    (if verbose? (println game))
    (let [guess (.nextGuess strategy game)]
      (.makeGuess guess game)))
  (if verbose? (println game))
  (.currentScore game))

(defn average-vals [data key]
  (let [vals (map key (vals data))]
    (float (/ (reduce + vals) (count vals)))))

(defn run-words
  ([make-strategy
    arity->dictionary
    arity->letter->count
    additional-arguments
    words]
     (run-words make-strategy
                arity->dictionary
                arity->letter->count
                additional-arguments
                words
                false))
  ([make-strategy
   arity->dictionary
   arity->letter->count
   additional-arguments
   words
   verbose?]
     (let [time-values
           (map (fn [word]
                  (let [arity (count word)
                        [time value]
                        (time-and-value
                         (run (new HangmanGame word 4)
                              (make-strategy
                               (arity->dictionary arity)
                               (arity->letter->count arity)
                               (additional-arguments arity))
                              verbose?))]
                    {:time-in-ms time
                     :score value}))
                words)
           data (zipmap words time-values)
           average-time (average-vals data :time-in-ms)
           average-score (average-vals data :score)]
       {:data (into (sorted-map) data)
        :average-time-in-ms average-time
        :average-score average-score})))
