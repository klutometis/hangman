(ns hangman.predicate-strategy
  (:use
   [clojure.contrib.generic.functor :only (fmap)]
   [clojure.contrib.math :only (abs)]
   [clojure.set :only (map-invert)]
   [clojure.contrib.io :only (reader)]
   [clojure.pprint :only (pprint)])
  (:import (com.factual.hangman
            GuessingStrategy
            GuessLetter
            GuessWord
            HangmanGame)
           java.lang.Character))

(def- wildcard-predicate (constantly true))

(defn- positive-predicate [predicans]
  (fn [predicandum] (= predicans predicandum)))

(defn- negative-predicate [predicans]
  (fn [predicandum] (not (= predicans predicandum))))

(defn- string->predicates [string default-predicate]
  (map (fn [char]
         (if (= char HangmanGame/MYSTERY_LETTER)
           default-predicate
           (positive-predicate (char->letter char))))
       string))

(defn- every-predicate? [predicates word]
  (every? true?
          (map (fn [letter predicate] (predicate letter))
               word
               predicates)))

(defn- filter-predicate-dictionary [dictionary guessed-so-far last-guess]
  (let [predicates (string->predicates guessed-so-far (negative-predicate last-guess))]
    (filter (partial every-predicate? predicates) dictionary)))

(defn make-arity->predicate-dictionary [file]
  (with-open [input (reader file)]
    (binding [*in* input]
      (loop [string (read-line)
             arity->dictionary {}]
        (if string
          (recur (read-line)
                 (let [arity (count string)
                       ;; Using a set instead of a list here to take
                       ;; advantage of `disj'. Never mind: filter
                       ;; reconverts to lazy seq.
                       dictionary (arity->dictionary arity '())]
                   (assoc arity->dictionary
                     arity
                     (cons (string->word string)
                           dictionary))))
          arity->dictionary)))))

(defn make-sampling-predicate-strategy
  [initial-dictionary initial-letter->count]
  (make-frequency-strategy
   filter-predicate-dictionary
   sampling-count-letters
   remove-word
   char->letter
   letter->char
   word->string
   identity
   initial-dictionary
   initial-letter->count))
