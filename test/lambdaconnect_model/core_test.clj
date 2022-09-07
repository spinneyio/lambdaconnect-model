(ns lambdaconnect-model.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [lambdaconnect-model.core :as cc]
            [lambdaconnect-model.tools :as mpt]
            [lambdaconnect-model.transformations :as tr]
            [clojure.data.json :refer [read-str]]
            [clojure.spec.gen.alpha :as gen]))

(deftest test-core-data-xml-conversion
  (testing "Reading model file one"
    (let [model (cc/entities-by-name "resources/model.xml")]
      (is (= (count model) 4))
      (testing ";Json to model converter"
        (cc/specs model {:LAUser/gender #(s/gen #{"M" "F" "U"})
                         :LAUser/email (fn [] (gen/fmap #(str % "@test.com") (gen/string-alphanumeric)))})
        (try
          (let [game-model (get model "LAGame")
                json (-> "resources/fixtures.json"
                         slurp
                         read-str)
                ent (tr/json-to-clojure (-> json (get "LAGame") first) game-model)
                generated-games (gen/sample (s/gen (cc/spec-for-name :LAGame)) 200)]

            (is (s/valid? (cc/spec-for-name :LAGame) ent) (s/explain-str (cc/spec-for-name :LAGame) ent))
            (testing ";Inverse"
              (is (= (tr/clojure-to-json ent game-model) (-> json (get "LAGame") first)))
              (doseq [generated-game generated-games]
                (let [processed-game (-> generated-game
                                         (tr/clojure-to-json game-model)
                                         (tr/json-to-clojure game-model))]
                  (is (mpt/compare-objects generated-game processed-game game-model))))))
          (catch clojure.lang.ExceptionInfo e
            (.printStackTrace e))))))

  (testing "Schema from model"
    (let [model (cc/entities-by-name "resources/model.xml")
          schema (cc/datomic-schema model)]
      (is (= (+ 37 (count model)) (count schema)))))

  (testing "Specs"
    (cc/specs (cc/entities-by-name "resources/model.xml"))))