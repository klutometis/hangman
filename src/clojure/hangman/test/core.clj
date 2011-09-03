(ns hangman.test.core
  (:use [hangman.core])
  (:use [clojure.test])
  (:use [hangman.frequency-strategy])
  (:use [hangman.trie-strategy]))

(let [word1 '(1 2 3 4 5)
      word2 '(1 2 3 7 8)
      word3 '(9 2 3 7 8)
      trie (reduce merge-tries {} (map plumb (list word1 word2 word3)))]
  
  (deftest test-merge-tries
    (is trie {9 {2 {3 {7 {8 trie-sentinel}}}},
              1 {2 {3 {7 {8 trie-sentinel},
                       4 {5 trie-sentinel}}}}}))
  
   (deftest test-search-and-dissoc
     (testing "wildcard-predicate"
       (is (search-and-dissoc
            trie
            [wildcard-predicate
             wildcard-predicate
             wildcard-predicate
             wildcard-predicate
             wildcard-predicate])
           {1 {2 {3 {4 {5 trie-sentinel}
                     7 {8 trie-sentinel}}}}
            9 {2 {3 {7 {8 trie-sentinel}}}}}))
     (testing "negative-predicate"
       (is (search-and-dissoc
            trie
            [wildcard-predicate
             wildcard-predicate
             wildcard-predicate
             (negative-predicate 4)
             wildcard-predicate])
           {1 {2 {3 {7 {8 trie-sentinel}}}}
            9 {2 {3 {7 {8 trie-sentinel}}}}}))
     (testing "positive-predicate"
       (is (search-and-dissoc
            trie
            [(positive-predicate 1)
             wildcard-predicate
             wildcard-predicate
             wildcard-predicate
             wildcard-predicate])
           {1 {2 {3 {4 {5 trie-sentinel}
                     7 {8 trie-sentinel}}}}})))

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
