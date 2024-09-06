(ns lambdaconnect-model.utils-test
  (:require #?@(:clj [[clojure.test :refer [deftest is testing]]
                      [lambdaconnect-model.utils :refer [log-with-fn]]]
                :cljs [[cljs.test :refer [deftest is testing]]])
            [lambdaconnect-model.tools :refer [relevant-tags]])
  #?(:cljs (:require-macros [lambdaconnect-model.utils :refer [log-with-fn]])))

(deftest relevant-tags-test
  (let [constraint-to-be-tested '(and
                                 [= :tmp-field :tag0]
                                 (or
                                  [= :tmp-field :tag1]
                                  (and
                                   (or-join [= :tmp-field  :tag4] [= :tmp-field :tag2])
                                   (not [= :tmp-field :tag3]))))
        expected-results (reduce (fn [cur-set new-idx] (conj cur-set (keyword (str "tag" new-idx)))) #{} (range 5))]
    (is (= (relevant-tags constraint-to-be-tested) expected-results))
    (is (= (relevant-tags :all) #{}))
    (is (= (relevant-tags :none) #{}))))

(deftest log-with-fn-test
  (testing "Should not crash"
    (log-with-fn nil (assert false "This crash should never happen"))))
