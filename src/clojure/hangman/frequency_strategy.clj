(ns hangman.frequency-strategy
  ^{:doc "Some abstractions for the frequency-strategy which are
  common to e.g. regex-, predicate- and trie-strategies."}
  (:use [clojure.contrib.io :only (reader)]
        [clojure.contrib.generic.functor :only (fmap)]
        [clojure.string :only (join)]
        [clojure.set :only (difference map-invert)]
        [hangman.core :only (debug)])
  (:import (com.factual.hangman
            HangmanGame
            GuessingStrategy
            GuessWord
            GuessLetter)))

(defn letter->count-->letter->percentage [letter->count]
  "Convert a map of letter-counts to letter-percentages."
  (let [total (apply + (vals letter->count))]
    (fmap #(float (/ % total)) letter->count)))

(def ^{:doc "Threshold below which changes in percentage are
  considered stable"}
  *delta-percentage-tolerance* 1.0e-5)

(def ^{:doc "Frequency at which to sample delta-percentage"}
  *sampling-frequency* 100)

(def ^{:doc "Minimum iterations at which to test delta-percentage,
  expression as a ratio of the size of the dictionary"}
  *ratio-to-n-of-minimum-iterations* 100)

;;; Should last-letter->count be from *sampling-frequency* iterations
;;; or one iteration ago?
(defn sufficiently-stable? [last-letter->count letter->count]
  "Is the change in percentage of letters counts belove
*delta-percentage-tolerance*?"
  (let [last-letter->percentage (letter->count-->letter->percentage last-letter->count)
        letter->percentage (letter->count-->letter->percentage letter->count)
        delta-letter->percentage
        (merge-with - letter->percentage last-letter->percentage)]
    (< (apply + (vals delta-letter->percentage))
       *delta-percentage-tolerance*)))

(defn deterministic-count-letters [dictionary]
  "Count letters deterministically, i.e. exhaustively."
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
  "Count the letters probabilistically, and stop when
`sufficiently-stable?' is true."
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
  "For debugging, convert a map of integer-counts to char-counts."
  (reduce (fn [char->count [letter count]]
            (assoc char->count
              (letter->char letter)
              count))
          (sorted-map)
          letter->count))

(def wildcard-predicate
  ^{:doc "True w.r.t. all letters."}
  (constantly true))

(defn positive-predicate [predicans]
  "True w.r.t. certain letters."
  (fn [predicandum] (= predicans predicandum)))

(defn negative-predicate [predicans]
  "False w.r.t. certain letters."
  (fn [predicandum] (not (= predicans predicandum))))

(defn string->predicates [string default-predicate]
  "Take a string-encoded game state and convert it into positive and
negative or wildcard predicates, depending on need."
  (map (fn [char]
         (if (= char HangmanGame/MYSTERY_LETTER)
           default-predicate
           (positive-predicate (char->letter char))))
       string))

(defn make-arity->letter->count [count-letters arity->dictionary]
  "Create a map of word-arity to letter-counts."
  (fmap count-letters arity->dictionary))

(defn make-arity->deterministic-letter->count [arity->dictionary]
  "Create a map of word-arity to letter-counts deterministically."
  (make-arity->letter->count deterministic-count-letters arity->dictionary))

(defn make-arity->sampling-letter->count [arity->dictionary]
  "Create a map of word-arity to letter-counts probabilistically."
  (make-arity->letter->count sampling-count-letters arity->dictionary))

;;;; The abstract frequency strategy, basis for regex- and
;;;; predicate-strategies.

(defn make-arity->dictionary [reduce file]
  "Create a mapping from word-arity to dictionary."
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

(def *default-random-string-length* 8)

(defn random-string
  ([] (random-string *default-random-string-length*))
  ([length]
     (join (take length (map letter->char (shuffle (range 26)))))))

(defn make-frequency-strategy
  "Frequency strategies guess letters according to the most common
letters, and guess words when the remaining-words/remaining-guesses
ratio looks auspicious."
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
            (if (zero? n-words)
              ;; Whoops: we have a word here which doesn't appear to
              ;; be in the dictionary. Let's keep guessing randomly
              ;; until the game is over.
              (new GuessWord (random-string))
              (let [remaining-guesses (.numWrongGuessesRemaining game)]
                (if (<= n-words remaining-guesses)
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
                    (new GuessLetter (letter->char next-guess))))))))))))
