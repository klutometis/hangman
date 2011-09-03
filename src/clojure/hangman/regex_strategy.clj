(ns hangman.regex-strategy
  (:use [hangman.frequency-strategy :only
         (make-arity->dictionary
          make-frequency-strategy
          remove-word
          deterministic-count-letters
          sampling-count-letters)]
        [clojure.contrib.string :only (replace-str)])
  (:import (com.factual.hangman
            HangmanGame)))

(defn make-arity->regex-dictionary [file]
  (make-arity->dictionary
   #(cons %2 %1)
   file))

(defn negative-regex [last-guess]
  (format "[^%s]" last-guess))

(defn filter-regex-dictionary [dictionary guessed-so-far last-guess]
  (let [negative-regex (negative-regex last-guess)
        filtering-regex
        (format "^%s$" (replace-str (str HangmanGame/MYSTERY_LETTER) negative-regex guessed-so-far))
        filtering-pattern (re-pattern filtering-regex)]
    (filter #(re-seq filtering-pattern %) dictionary)))

(defn make-regex-strategy
  [count-letters initial-dictionary initial-letter->count & rest]
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
  (partial make-regex-strategy deterministic-count-letters))
