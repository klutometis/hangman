(ns hangman.predicate-strategy
  ^{:doc "Predicate-strategy for hangman that applies ad-hoc
  predicates to integer-encode strings."}
  (:use
   [clojure.contrib.io :only (reader)]
   [hangman.frequency-strategy
    :only (letter->char
           char->letter
           string->word
           word->string
           make-frequency-strategy
           sampling-count-letters
           remove-word
           string->predicates
           negative-predicate)]))

(defn every-predicate? [predicates word]
  "Tests whether `word' satisifies `predicates'; superstrings that
share a common prefix will satisfy `predicates'."
  (every? true?
          (map (fn [letter predicate] (predicate letter))
               word
               predicates)))

(defn filter-predicate-dictionary [dictionary guessed-so-far last-guess]
  "Filter `dictionary' according to predicates gleaned from
`guessed-so-far' and `last-guess'."
  (let [predicates (string->predicates guessed-so-far (negative-predicate last-guess))]
    (filter (partial every-predicate? predicates) dictionary)))

(defn make-arity->predicate-dictionary [file]
  "Make a mapping from word-arity to an integer-encoded dictionary
suitable for ad-hoc predicates."
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
  "Make a predicate-strategy with a sampling letter-counter."
  [initial-dictionary initial-letter->count & rest]
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
