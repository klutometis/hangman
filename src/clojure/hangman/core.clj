(ns hangman.core
  (:import (com.factual.hangman
            HangmanGame
            HangmanGame$Status))
  (:use [clojure.contrib.pprint :only (pprint)]))

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

(defn run-with-out-str [game strategy verbose?]
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
  (let [vals (map key (vals data))]
    (float (/ (reduce + vals) (count vals)))))

(defn run-words
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
