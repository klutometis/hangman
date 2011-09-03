(ns hangman.test.core
  (:use [hangman.core]
        [clojure.test]
        [hangman.frequency-strategy]
        [hangman.trie-strategy])
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
