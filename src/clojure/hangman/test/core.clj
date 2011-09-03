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
              '((1 2 3 4 5) (9 2 3 7 8))))))

   ;; (assert (= (count-trie trie)
   ;;            {9 1, 1 1, 3 2, 8 2, 7 2, 2 2, 5 1, 4 1}))
   ;; (assert (search-trie trie word1))
   ;; (assert (search-trie trie word2))
   ;; (assert (search-trie trie word3))
   ;; (assert (not (search-trie trie '(1 2 3 4 6))))
   ;; (assert (= (search-and-dissoc trie '(1 2 4))
   ;;            {9 {2 {3 {7 {8 trie-sentinel}}}},
   ;;             1 {2 trie-sentinel}}))
   ;; (assert (trie-sentinel? (search-and-dissoc trie '(2))))
   ;; (assert (= (search-and-dissoc trie `(~wildcard 2 4))
   ;;            {9 {2 trie-sentinel}, 1 {2 trie-sentinel}}))
   ;; (assert (=
   ;;          (search-and-dissoc-predicate
   ;;           trie
   ;;           [wildcard-predicate
   ;;            (positive-predicate 2)
   ;;            (positive-predicate 4)])
   ;;          {1 {2 trie-sentinel}, 9 {2 trie-sentinel}}))
   ;; (assert (= (get-words trie)
   ;;            '((5 4 3 2 1)
   ;;              (8 7 3 2 1)
   ;;              (8 7 3 2 9))))
   ;; (assert (= (search-and-dissoc-predicate
   ;;             trie
   ;;             [wildcard-predicate
   ;;              wildcard-predicate
   ;;              wildcard-predicate
   ;;              (negative-predicate 4)])
   ;;            {1 {2 {3 {7 {8 trie-sentinel}}}},
   ;;             9 {2 {3 {7 {8 trie-sentinel}}}}}))
   ;; (assert (every-predicate? [(positive-predicate 1)
   ;;                            wildcard-predicate
   ;;                            (negative-predicate 2)
   ;;                            (positive-predicate 4)
   ;;                            (positive-predicate 5)]
   ;;                           '(1 2 3 4 5)))
   ;; (assert (= (get-words-with-predicates
   ;;              trie
   ;;              [(positive-predicate 1)
   ;;               (positive-predicate 2)
   ;;               (positive-predicate 3)
   ;;               wildcard-predicate
   ;;               wildcard-predicate])
   ;;            '((1 2 3 4 5)
   ;;              (1 2 3 7 8))))
   ;; (get-words-with-predicates-and-max
   ;;   trie
   ;;   [(positive-predicate 1)
   ;;    (positive-predicate 2)
   ;;    (positive-predicate 3)
   ;;    wildcard-predicate
   ;;    wildcard-predicate]
   ;;   3)
   ;; (get-words-with-predicates-and-max
   ;;   trie
   ;;   [wildcard-predicate
   ;;    wildcard-predicate
   ;;    wildcard-predicate
   ;;    wildcard-predicate
   ;;    wildcard-predicate]
   ;;   2)
   ;; (with-call-cc
   ;;   (let-cc
   ;;    k
   ;;    (get-words-with-predicates-and-max-and-continuation
   ;;      k
   ;;      trie
   ;;      [wildcard-predicate
   ;;       wildcard-predicate
   ;;       wildcard-predicate
   ;;       wildcard-predicate
   ;;       wildcard-predicate]
   ;;      2)))
)
