(ns hangman.regex-strategy
  ^{:doc "Regex-strategies encode their dictionaries as strings,
  filter linearly with regexen: deterministic regex counts letters
  deterministically; sampling regex, probabilistically."}
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
  "Make a map from word-arity to dictionary."
  (make-arity->dictionary
   #(cons %2 %1)
   file))

(defn negative-regex [last-guess]
  "Make a regex which negates the given letter."
  (format "[^%s]" last-guess))

(defn filter-regex-dictionary [dictionary guessed-so-far last-guess]
  "Filter the dictionary linearly against a given regex, composed
negatively from `last-guess'; positively from `guessed-so-far'."
  (let [negative-regex (negative-regex last-guess)
        filtering-regex
        (format "^%s$" (replace-str (str HangmanGame/MYSTERY_LETTER) negative-regex guessed-so-far))
        filtering-pattern (re-pattern filtering-regex)]
    (filter #(re-seq filtering-pattern %) dictionary)))

(defn make-regex-strategy
  "Make a linear strategy, which filters on word-lists against
regexen."
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
  ^{:doc "Make a regex strategy that counts letters deterministically."}
  (partial make-regex-strategy deterministic-count-letters))

(def make-sampling-regex-strategy
  ^{:doc "Make a regex strategy that counts letters samplingly."}
  (partial make-regex-strategy deterministic-count-letters))
