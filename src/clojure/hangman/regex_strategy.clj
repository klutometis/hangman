(ns hangman.regex-strategy
  (:import (com.factual.hangman
            GuessingStrategy
            GuessLetter
            GuessWord
            HangmanGame
            HangmanGame$Status)
           java.lang.Character)
  (:use [clojure.contrib.io :only (reader)]
        [clojure.contrib.string :only (replace-str)]
        [clojure.set :only (difference map-invert)]
        [clojure.contrib.math :only (abs)]
        [clojure.pprint :only (pprint)]
        [clojure.contrib.generic.functor :only (fmap)]
        hangman.frequency-strategy))

(defn make-arity->regex-dictionary [dictionary-file]
  (with-open [dictionary-input (reader dictionary-file)]
    (binding [*in* dictionary-input]
      (loop [word (read-line)
             arity->dictionary {}]
        (if word
          (let [arity (count word)]
            (recur (read-line)
                   (assoc arity->dictionary arity (cons word (arity->dictionary arity '())))))
          arity->dictionary)))))

(defn- negative-regex [last-guess]
  (format "[^%s]" last-guess))

(defn filter-regex-dictionary [dictionary guessed-so-far last-guess]
  (let [negative-regex (negative-regex last-guess)
        filtering-regex (format "^%s$" (replace-str "-" negative-regex guessed-so-far))
        filtering-pattern (re-pattern filtering-regex)]
    (filter #(re-seq filtering-pattern %) dictionary)))

(defn make-regex-strategy
  [count-letters initial-dictionary initial-letter->count]
  (make-frequency-strategy
   filter-regex-dictionary
   count-letters
   remove-word
   identity
   identity
   identity
   identity
   initial-dictionary
   initial-letter->count))

(def make-deterministic-regex-strategy
  (partial make-regex-strategy deterministic-count-letters))

(def make-sampling-regex-strategy
  (partial make-regex-strategy sampling-count-letters))
