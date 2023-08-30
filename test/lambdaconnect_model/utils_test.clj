(ns lambdaconnect-model.utils-test
  (:require [clojure.test :refer [deftest is]]
            [lambdaconnect-model.utils :refer [relevant-tags]]))

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
