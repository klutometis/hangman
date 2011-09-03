(ns hangman.trie-strategy
  (:use [hangman.frequency-strategy :only
         (string->predicates
           make-arity->dictionary
           make-frequency-strategy
           negative-predicate
           char->letter
           letter->char
           word->string
           string->word
           make-arity->letter->count)]
        [hangman.core :only
         (debug)]))

(def trie-sentinel (gensym))

(defn trie-sentinel? [object]
  (= object trie-sentinel))

(defn plumb
  ([word] (plumb {} word))
  ([trie word]
     (if word
       (let [letter (first word)]
         (assoc (trie letter {})
           letter
           (plumb trie
                  (next word))))
       trie-sentinel)))

(defn count-trie
  ([trie] (count-trie {} trie))
  ([frequencies trie]
     (if (not (trie-sentinel? trie))
       (merge-with
        +
        frequencies
        (reduce
         (fn [frequencies key]
           (merge-with
            frequencies
            (count-trie frequencies (trie key))))
         (let [keys (keys trie)]
           (zipmap
            keys
            (replicate (count keys) 1)))
         (keys trie)))
       frequencies)))

;;; Really what we need to do is a recursive merge of some sort: the
;;; problem is, we have to account for all keys; including ones which
;;; are currently different and future divergence, too. Ouch.
;;;
;;; No, that's not the case (although I wonder if we could dynamically
;;; program this to avoid exponential complexity): the problem is a
;;; multi-key mergens with a single-key mergendum. Therefore, the
;;; first-key hack should abide (, dude).
(defn merge-tries [mergens mergendum]
  (cond (or (trie-sentinel? mergens) (empty? mergens)) mergendum
        (or (trie-sentinel? mergendum) (empty? mergendum)) mergens
        :else (let [mergendum-key (first (keys mergendum))
                    submergens (mergens mergendum-key)
                    submergendum (mergendum mergendum-key)]
                (if (mergendum mergendum-key)
                  (assoc mergens mergendum-key
                         (merge-tries submergens submergendum))
                  (assoc mergens mergendum-key
                         (mergendum mergendum-key))))))

(defn dissoc-trie [trie word]
  (if word
    (let [letter (first word)
          subtrie (trie letter)]
      (if (trie-sentinel? subtrie)
        trie-sentinel
        (assoc trie letter (dissoc-trie subtrie (next word)))))
    trie-sentinel))

(defn search-and-dissoc [trie predicates]
  (if (or (empty? predicates)
          (trie-sentinel? trie))
    trie
    (let [predicate (first predicates)
          letters (filter predicate (keys trie))]
      (if (empty? letters)
        trie-sentinel
        (reduce
         (fn [subtrie letter]
           (assoc subtrie
             letter
             (search-and-dissoc (trie letter) (next predicates))))
         {}
         letters)))))

(defn get-words
  ([arity trie] (get-words arity trie '() '()))
  ([arity trie words word]
     (if (trie-sentinel? trie)
       (if (= (count word) arity)
         (list (reverse word))
         nil)
       (reduce
        (fn [words letter]
          (concat (get-words arity (trie letter) words (cons letter word)) words))
        '()
        (keys trie)))))

(defn make-arity->trie-dictionary [file]
  (make-arity->dictionary
   #(merge-tries %1 (plumb (string->word %2)))
   file))

(defn make-arity->trie-letter->count [arity->dictionary]
  (make-arity->letter->count count-trie arity->dictionary))

(defn make-trie-strategy [initial-dictionary initial-letter->count & arity]
  (make-frequency-strategy
   (fn [dictionary guessed-so-far last-guess]
     (let [predicates (string->predicates guessed-so-far (negative-predicate last-guess))]
       (search-and-dissoc dictionary predicates)))
   count-trie
   (fn [word dictionary]
     (dissoc-trie dictionary word))
   char->letter
   letter->char
   word->string
   (partial get-words arity)
   initial-dictionary
   initial-letter->count))
