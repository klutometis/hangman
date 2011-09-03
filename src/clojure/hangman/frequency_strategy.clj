(ns hangman.frequency-strategy
  (:use [clojure.contrib.io :only (reader)]
        [clojure.contrib.generic.functor :only (fmap)]
        [clojure.string :only (join)]
        [clojure.set :only (difference map-invert)])
  (:import (com.factual.hangman
            HangmanGame
            GuessingStrategy
            GuessWord
            GuessLetter)))

(defn letter->count-->letter->percentage [letter->count]
  (let [total (apply + (vals letter->count))]
    (fmap #(float (/ % total)) letter->count)))

(def *delta-percentage-tolerance* 1.0e-5)

(def *sampling-frequency* 100)

(def *ratio-to-n-of-minimum-iterations* 100)

;;; Should last-letter->count be from *sampling-frequency* iterations
;;; or one iteration ago?
(defn sufficiently-stable? [last-letter->count letter->count]
  (let [last-letter->percentage (letter->count-->letter->percentage last-letter->count)
        letter->percentage (letter->count-->letter->percentage letter->count)
        delta-letter->percentage
        (merge-with - letter->percentage last-letter->percentage)]
    (< (apply + (vals delta-letter->percentage))
       *delta-percentage-tolerance*)))

(defn deterministic-count-letters [dictionary]
  (loop [dictionary dictionary
         letter->frequency (hash-map)]
    (if dictionary
      (let [word (first dictionary)
            ;; `distinct' was here.
            letters word]
        (recur (next dictionary)
               (reduce (fn [letter->frequency letter]
                         (assoc letter->frequency
                           letter
                           (+ (letter->frequency letter 0) 1)))
                       letter->frequency
                       letters)))
      letter->frequency)))

(defn sampling-count-letters [dictionary]
  (let [minimum-iterations
        (/ (count dictionary) *ratio-to-n-of-minimum-iterations*)]
    (loop [last-letter->count {}
           letter->count {}
           dictionary (shuffle dictionary)
           iteration 0]
      (if dictionary
        (if (and (> iteration minimum-iterations)
                 (zero? (mod iteration *sampling-frequency*))
                 (sufficiently-stable? last-letter->count letter->count))
          letter->count
          (let [word (first dictionary)]
            (recur letter->count
                   (merge-with
                    +
                    letter->count
                    (zipmap word (replicate (count word) 1)))
                   (next dictionary)
                   (inc iteration))))
        letter->count))))

;;;; Some utilities common to trie- and predicate-based strategies.

(defn remove-word [word dictionary]
  (remove #(= % word) dictionary))

(defn char->letter [char]
  (- (int char) 97))

(defn string->word [string]
  (map char->letter string))

(defn letter->char [letter]
  (char (+ 97 letter)))

(defn word->string [word]
  (join (map letter->char word)))

(defn words->strings [words]
  (map word->string words))

(defn letter->count-->char->count [letter->count]
  (reduce (fn [char->count [letter count]]
            (assoc char->count
              (letter->char letter)
              count))
          (sorted-map)
          letter->count))

(def wildcard-predicate (constantly true))

(defn positive-predicate [predicans]
  (fn [predicandum] (= predicans predicandum)))

(defn negative-predicate [predicans]
  (fn [predicandum] (not (= predicans predicandum))))

(defn string->predicates [string default-predicate]
  (map (fn [char]
         (if (= char HangmanGame/MYSTERY_LETTER)
           default-predicate
           (positive-predicate (char->letter char))))
       string))

(defn make-arity->letter->count [count-letters arity->dictionary]
  (fmap count-letters arity->dictionary))

(defn make-arity->deterministic-letter->count [arity->dictionary]
  (make-arity->letter->count deterministic-count-letters arity->dictionary))

(defn make-arity->sampling-letter->count [arity->dictionary]
  (make-arity->letter->count sampling-count-letters arity->dictionary))

;;;; The abstract frequency strategy, basis for regex- and
;;;; predicate-strategies.

(defn make-arity->dictionary [reduce file]
  (with-open [input (reader file)]
    (binding [*in* input]
      (loop [word (read-line)
             arity->dictionary {}]
        (if word
          (recur (read-line)
                 (let [arity (count word)
                       dictionary (arity->dictionary arity)]
                   (assoc arity->dictionary
                     arity
                     (reduce dictionary word))))
          arity->dictionary)))))

(defn make-frequency-strategy
  [filter-dictionary
   count-letters
   remove-word
   char->letter
   letter->char
   word->string
   get-words
   initial-dictionary
   initial-letter->count]
  (let [dictionary (atom initial-dictionary)
        letter->count (atom initial-letter->count)
        last-guess (atom nil)]
    (reify
      GuessingStrategy
      (nextGuess [_ game]
        (do
          (if @last-guess
            (let [guessed-so-far (.toLowerCase (.getGuessedSoFar game))]
              (reset! dictionary (filter-dictionary @dictionary guessed-so-far @last-guess))
              (reset! letter->count (count-letters @dictionary))))
          (let [words (get-words @dictionary)
                n-words (count words)]
            (let [remaining-guesses (.numWrongGuessesRemaining game)]
              (if (and (pos? n-words)
                       (<= n-words remaining-guesses))
                (let [word (nth words (rand-int n-words))]
                  (reset! dictionary (remove-word word @dictionary))
                  (reset! last-guess nil)
                  (new GuessWord (word->string word)))
                (let [guessed-letters
                      (map #(-> % Character/toLowerCase char->letter)
                           (.getAllGuessedLetters game))
                      ;; Devaluate letters already-guessed.
                      letter->count
                      (merge @letter->count
                             (zipmap guessed-letters (replicate (count guessed-letters) 0)))
                      count->letter
                      (into (sorted-map-by >) (map-invert letter->count))
                      next-guess
                      (count->letter (apply max (keys count->letter)))]
                  (reset! last-guess next-guess)
                  (new GuessLetter (letter->char next-guess)))))))))))
