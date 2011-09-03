(ns hangman.main
  (:use [clojure.contrib.command-line :only (with-command-line)]
        [hangman.regex-strategy :only
         (make-deterministic-regex-strategy
          make-sampling-regex-strategy
          make-arity->regex-dictionary)]
        [hangman.frequency-strategy :only
         (deterministic-count-letters
          sampling-count-letters
          make-arity->deterministic-letter->count
          make-arity->sampling-letter->count)]
        [hangman.predicate-strategy :only
         (make-sampling-predicate-strategy
          make-arity->predicate-dictionary)]
        [hangman.trie-strategy :only
         (make-trie-strategy
          make-arity->trie-dictionary
          count-trie
          make-arity->trie-letter->count)]
        [clojure.contrib.pprint :only (pprint)]
        [clojure.contrib.io :only (reader)]
        [hangman.core :only (run-words)]))

(defn -main [& args]
  (let [args (or args ["--help"])]
    (with-command-line args
      "Usage: hangman [--deterministic-regex|--sampling-regex|--predicate|--trie] [--max-wrong-guesses|-m GUESSES] [--all|-a] [-v|--verbose] DICTIONARY [WORD]..."
      [[deterministic-regex? d? "Use the deterministic regex strategy." true]
       [sampling-regex? s? "Use the sampling regex strategy." false]
       [predicate? p? "Use the predicate strategy." false]
       [trie? t? "Use the trie strategy." false]
       [max-wrong-guesses m "Set max wrong guesses to GUESSES." 4]
       [all? a? "Run all the words in the dictionary." false]
       [verbose? v? "Verbose output (NB: affects reported times)" false]
       dictionary-words]
      (let [[dictionary & words] dictionary-words
            default-make-strategy make-deterministic-regex-strategy
            default-make-arity->dictionary make-arity->regex-dictionary
            default-make-arity->letter->count make-arity->deterministic-letter->count
            default-additional-arguments (constantly nil)]
        ;; Structure this as a map, instead.
        (let [make-strategy
              (cond deterministic-regex? make-deterministic-regex-strategy
                    sampling-regex? make-sampling-regex-strategy
                    predicate? make-sampling-predicate-strategy
                    trie? make-trie-strategy
                    :else default-make-strategy)
              make-arity->dictionary
              (cond deterministic-regex? make-arity->regex-dictionary
                    sampling-regex? make-arity->regex-dictionary
                    predicate? make-arity->predicate-dictionary
                    trie? make-arity->trie-dictionary
                    :else default-make-arity->dictionary)
              make-arity->letter->count
              (cond deterministic-regex? make-arity->deterministic-letter->count
                    sampling-regex? make-arity->sampling-letter->count
                    predicate? make-arity->deterministic-letter->count
                    trie? make-arity->trie-letter->count
                    :else default-make-arity->letter->count)
              additional-arguments
              (cond deterministic-regex? (constantly nil)
                    sampling-regex? (constantly nil)
                    predicate? (constantly nil)
                    trie? identity
                    :else default-additional-arguments)]
          (let [arity->dictionary (make-arity->dictionary dictionary)
                arity->letter->count (make-arity->letter->count arity->dictionary)
                words (if all? (line-seq (reader dictionary)) words)]
            (pprint (run-words make-strategy
                               arity->dictionary
                               arity->letter->count
                               additional-arguments
                               max-wrong-guesses
                               words
                               verbose?))))))))