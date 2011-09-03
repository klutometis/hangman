(ns hangman.test.core
  (:use [hangman.core]
        [clojure.test]
        [hangman.frequency-strategy]
        [hangman.trie-strategy]
        [hangman.regex-strategy]
        [hangman.predicate-strategy])
  (:import (com.factual.hangman
            HangmanGame)))

(let [word1 '(1 2 3 4 5)
      word2 '(1 2 3 7 8)
      word3 '(9 2 3 7 8)
      trie (reduce merge-tries {} (map plumb (list word1 word2 word3)))]
  
  (deftest test-count-trie
    (is (= {9 1, 1 1, 3 2, 8 2, 7 2, 2 2, 5 1, 4 1}
           (count-trie trie))))

  (deftest test-plumb
    (let [word (string->word "factual")]
      (is (= word '(5 0 2 19 20 0 11)))
      (is (= (plumb word)
             {5 {0 {2 {19 {20 {0 {11 trie-sentinel}}}}}}}))))

  (deftest test-merge-tries
    (is (= trie
           {9 {2 {3 {7 {8 trie-sentinel}}}},
            1 {2 {3 {7 {8 trie-sentinel},
                     4 {5 trie-sentinel}}}}})))
  
   (deftest test-search-and-dissoc
     (testing "wildcard-predicate"
       (is (=
            (search-and-dissoc
             trie
             [wildcard-predicate
              wildcard-predicate
              wildcard-predicate
              wildcard-predicate
              wildcard-predicate])
            {1 {2 {3 {4 {5 trie-sentinel}
                      7 {8 trie-sentinel}}}}
             9 {2 {3 {7 {8 trie-sentinel}}}}})))
     (testing "negative-predicate"
       (is (=
            (search-and-dissoc
             trie
             [wildcard-predicate
              wildcard-predicate
              wildcard-predicate
              (negative-predicate 4)
              wildcard-predicate])
            {1 {2 {3 {7 {8 trie-sentinel}}}}
             9 {2 {3 {7 {8 trie-sentinel}}}}})))
     (testing "positive-predicate"
       (is (=
            (search-and-dissoc
             trie
             [(positive-predicate 1)
              wildcard-predicate
              wildcard-predicate
              wildcard-predicate
              wildcard-predicate])
            {1 {2 {3 {4 {5 trie-sentinel}
                      7 {8 trie-sentinel}}}}}))))

   (deftest test-get-words
     (is (= (get-words 5 trie)
            '((1 2 3 4 5) (1 2 3 7 8) (9 2 3 7 8))))

     (let [dissociated-trie
           (dissoc-trie trie '(1 2 3 7 8))]
       (is (= dissociated-trie
              {9 {2 {3 {7 {8 trie-sentinel}}}},
               1 {2 {3 {7 trie-sentinel,
                        4 {5 trie-sentinel}}}}}))
       (is (= (get-words 5 dissociated-trie)
              '((1 2 3 4 5) (9 2 3 7 8)))))))

(let [words '("sensitive" "fritters" "dyslexias" "secureness" "paragoning")
      word (first words)]
  (let [trie (reduce #(merge-tries %1 (plumb (string->word %2)))
                     {}
                     words)
        letter->count (count-trie trie)]
    (deftest test-trie-strategy
      (is (< (:score (run (new HangmanGame word 4)
                          (make-trie-strategy trie letter->count (count word))
                          false))
             25)))))

(let [dictionary '("sensitive" "fritters" "dyslexias" "secureness" "paragoning")
      letter->count (deterministic-count-letters dictionary)]
  (let [word (first dictionary)]
    (deftest test-filter-regex-dictionary
      (is (= (filter-regex-dictionary dictionary "---------" \a)
             '("sensitive"))))

    (deftest test-deterministic-regex-strategy
      (is (= (:score (run (new HangmanGame word 4)
                          (make-regex-strategy deterministic-count-letters dictionary letter->count)
                          false))
             1)))

    (deftest test-sampling-regex-strategy
      (is (= (:score (run (new HangmanGame word 4)
                          (make-regex-strategy sampling-count-letters dictionary letter->count)
                          false))
             1)))))

(let [words '("sensitive" "fritters" "dyslexias" "secureness" "paragoning")
      dictionary (map string->word words)
      letter->count (deterministic-count-letters dictionary)]
  (let [word (first dictionary)]
    (deftest test-filter-predicate-dictionary
      (is (= (filter-predicate-dictionary dictionary "---------" 0)
             ;; We have several here (as opposed to regex) because the
             ;; predicate-strategy is not arity-sensitive.
             '((18 4 13 18 8 19 8 21 4)
               (5 17 8 19 19 4 17 18)
               (18 4 2 20 17 4 13 4 18 18)))))

    (deftest test-sampling-predicate-strategy
      (is (= (:score (run (new HangmanGame (first words) 4)
                          (make-sampling-predicate-strategy dictionary letter->count)
                          false))
             1)))))
